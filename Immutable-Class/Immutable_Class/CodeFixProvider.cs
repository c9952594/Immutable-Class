using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using ImmutableClass;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.Rename;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using static Microsoft.CodeAnalysis.CSharp.SyntaxKind;

namespace Immutable_Class
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(ImmutableClassCodeFixProvider)), Shared]
    public class ImmutableClassCodeFixProvider : CodeFixProvider
    {
        const string Title = "As immutable class";

        public sealed override ImmutableArray<string> FixableDiagnosticIds => ImmutableArray.Create(ImmutableClassAnalyzer.DiagnosticId);

        public sealed override FixAllProvider GetFixAllProvider() => WellKnownFixAllProviders.BatchFixer;

        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var diagnostic = context.Diagnostics.First();
            var diagnosticSpan = diagnostic.Location.SourceSpan;

            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            var typeDecl = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType<TypeDeclarationSyntax>().First();

            context.RegisterCodeFix(
                CodeAction.Create(
                    Title,
                    cancellationToken => MakeImmutableAsync(context.Document, typeDecl, cancellationToken),
                    Title),
                diagnostic);
        }

        public static async Task<Solution> MakeImmutableAsync(Document document, TypeDeclarationSyntax typeDecl, CancellationToken cancellationToken)
        {
            var fieldDeclarations = typeDecl.DescendantNodes().OfType<FieldDeclarationSyntax>();
            var readonlyFieldDeclarations = fieldDeclarations
                .Where(field => field.Modifiers.Any(modifier => modifier.Kind() == ReadOnlyKeyword)).ToArray();

            var @class = new Class(typeDecl.Identifier.ValueText);

            var fields = readonlyFieldDeclarations.SelectMany(field =>
                field.Declaration.Variables.Select(variable =>
                    new Field(field.Declaration.Type, variable.Identifier.ValueText))).ToArray();

            var documentEditor = await DocumentEditor.CreateAsync(document, cancellationToken);

            var properties = GetProperties(fields).ToArray();
            var constructor = GetConstructor(@class, fields);
            var method = GetWith(@class, fields);
            var optional = GetOptional();

            var all = List<MemberDeclarationSyntax>(properties).Add(constructor).Add(method).Add(optional).ToArray();
            var surroundedByRegion = SurroundWithRegion(all);

            documentEditor.InsertAfter(readonlyFieldDeclarations.Last(), surroundedByRegion);

            return document.Project.Solution.WithDocumentText(
                document.Id,
                await documentEditor.GetChangedDocument().GetTextAsync(cancellationToken));
        }

        static StructDeclarationSyntax GetOptional()
            => StructDeclaration("Optional")
                .WithModifiers(TokenList(Token(PublicKeyword)))
                .WithTypeParameterList(
                    TypeParameterList(SingletonSeparatedList(TypeParameter(Identifier("T")))))
                .WithMembers(List(new MemberDeclarationSyntax[]
                {
                    PropertyDeclaration(IdentifierName("T"), Identifier("Value"))
                        .WithModifiers(TokenList(Token(PublicKeyword))).WithAccessorList(
                            AccessorList(SingletonList(
                                AccessorDeclaration(GetAccessorDeclaration)
                                    .WithSemicolonToken(Token(SemicolonToken))))),
                    PropertyDeclaration(PredefinedType(Token(BoolKeyword)), Identifier("HasValue"))
                        .WithModifiers(TokenList(Token(PublicKeyword)))
                        .WithAccessorList(AccessorList(SingletonList(
                            AccessorDeclaration(GetAccessorDeclaration).WithSemicolonToken(Token(SemicolonToken))))),
                    ConstructorDeclaration(Identifier("Optional")).WithModifiers(TokenList(Token(PublicKeyword)))
                        .WithParameterList(ParameterList(
                            SingletonSeparatedList(Parameter(Identifier("value"))
                                .WithType(IdentifierName("T"))))).WithBody(Block(
                            ExpressionStatement(AssignmentExpression(SimpleAssignmentExpression,
                                IdentifierName("Value"), IdentifierName("value"))),
                            ExpressionStatement(AssignmentExpression(SimpleAssignmentExpression,
                                IdentifierName("HasValue"), LiteralExpression(TrueLiteralExpression))))),
                    ConversionOperatorDeclaration(Token(ExplicitKeyword), IdentifierName("T"))
                        .WithModifiers(TokenList(Token(PublicKeyword), Token(StaticKeyword)))
                        .WithParameterList(ParameterList(SingletonSeparatedList(
                            Parameter(Identifier("optional")).WithType(GenericName(Identifier("Optional"))
                                .WithTypeArgumentList(
                                    TypeArgumentList(SingletonSeparatedList<TypeSyntax>(IdentifierName("T"))))))))
                        .WithExpressionBody(ArrowExpressionClause(MemberAccessExpression(SimpleMemberAccessExpression,
                            IdentifierName("optional"), IdentifierName("Value"))))
                        .WithSemicolonToken(Token(SemicolonToken)),
                    ConversionOperatorDeclaration(Token(ImplicitKeyword),
                            GenericName(Identifier("Optional"))
                                .WithTypeArgumentList(
                                    TypeArgumentList(SingletonSeparatedList<TypeSyntax>(IdentifierName("T")))))
                        .WithModifiers(TokenList(Token(PublicKeyword), Token(StaticKeyword)))
                        .WithParameterList(ParameterList(
                            SingletonSeparatedList(Parameter(Identifier("value"))
                                .WithType(IdentifierName("T"))))).WithExpressionBody(ArrowExpressionClause(
                            ObjectCreationExpression(GenericName(Identifier("Optional"))
                                    .WithTypeArgumentList(
                                        TypeArgumentList(SingletonSeparatedList<TypeSyntax>(IdentifierName("T")))))
                                .WithArgumentList(
                                    ArgumentList(
                                        SingletonSeparatedList(Argument(IdentifierName("value")))))))
                        .WithSemicolonToken(Token(SemicolonToken)),
                    MethodDeclaration(PredefinedType(Token(StringKeyword)), Identifier("ToString"))
                        .WithModifiers(TokenList(Token(PublicKeyword), Token(OverrideKeyword)))
                        .WithExpressionBody(ArrowExpressionClause(
                            InterpolatedStringExpression(Token(InterpolatedStringStartToken)).WithContents(
                                List(new InterpolatedStringContentSyntax[]
                                {
                                    InterpolatedStringText().WithTextToken(Token(TriviaList(),
                                        InterpolatedStringTextToken, "Optional (HasValue: ", "Optional (HasValue: ",
                                        TriviaList())),
                                    Interpolation(IdentifierName("HasValue")),
                                    InterpolatedStringText().WithTextToken(Token(TriviaList(),
                                        InterpolatedStringTextToken, ", Value: '", ", Value: '", TriviaList())),
                                    Interpolation(IdentifierName("Value")),
                                    InterpolatedStringText().WithTextToken(Token(TriviaList(),
                                        InterpolatedStringTextToken, "')", "')", TriviaList()))
                                })))).WithSemicolonToken(Token(SemicolonToken))
                })).NormalizeWhitespace().WithLeadingTrivia(CarriageReturnLineFeed);

        static IEnumerable<MemberDeclarationSyntax> SurroundWithRegion(MemberDeclarationSyntax[] all)
        {
            all[0] = all[0].WithLeadingTrivia(
                Trivia(RegionDirectiveTrivia(true)
                    .WithHashToken(Token(HashToken))
                    .WithRegionKeyword(Token(RegionKeyword)).WithEndOfDirectiveToken(
                        Token(
                            TriviaList(Space, PreprocessingMessage(@"Generated code: Immutable class")),
                            EndOfDirectiveToken,
                            TriviaList()))),
                CarriageReturnLineFeed
            );

            all[all.Length - 1] = all[all.Length - 1].WithTrailingTrivia(
                Trivia(
                    EndRegionDirectiveTrivia(true)
                        .WithLeadingTrivia(CarriageReturnLineFeed)
                        .WithTrailingTrivia(CarriageReturnLineFeed)));

            return all;
        }

        static IEnumerable<PropertyDeclarationSyntax> GetProperties(IEnumerable<Field> fields)
            => fields.Select(GetProperty);

        static PropertyDeclarationSyntax GetProperty(Field field)
            => PropertyDeclaration(field.Type, field.Name.AsProperty())
                .AddModifiers(Token(PublicKeyword).WithTrailingTrivia(Space))
                .WithExpressionBody(ArrowExpressionClause(IdentifierName(field.Name)))
                .WithSemicolonToken(Token(SemicolonToken))
                .WithTrailingTrivia(CarriageReturnLineFeed);

        static IEnumerable<ParameterSyntax> GetConstructorParameters(IEnumerable<Field> fields)
            => fields.Select(GetConstructorParameter);

        static ParameterSyntax GetConstructorParameter(Field field)
            => Parameter(Identifier(field.Name.AsParameter()))
                .WithType(field.Type);

        static IEnumerable<AssignmentExpressionSyntax> GetConstructorAssignments(IEnumerable<Field> fields)
            => fields.Select(GetConstructorAssignment);

        static AssignmentExpressionSyntax GetConstructorAssignment(Field field)
            => AssignmentExpression(
                SimpleAssignmentExpression,
                IdentifierName($"this.{field.Name}"),
                IdentifierName(field.Name.AsParameter()));

        static ConstructorDeclarationSyntax GetConstructor(Class @class, IEnumerable<Field> fields)
            => ConstructorDeclaration(@class.Name)
                .AddModifiers(Token(PublicKeyword).WithTrailingTrivia(Space))
                .AddParameterListParameters(GetConstructorParameters(fields).ToArray())
                .AddBodyStatements(GetConstructorAssignments(fields).Select(ExpressionStatement).ToArray())
                .WithTrailingTrivia(CarriageReturnLineFeed);

        static MethodDeclarationSyntax GetWith(Class @class, IEnumerable<Field> fields)
        {
            var locals = fields.Select((field, index) =>
                LocalDeclarationStatement(VariableDeclaration(IdentifierName("var")).WithVariables(
                    SingletonSeparatedList(VariableDeclarator(Identifier($"__local{index}").WithLeadingTrivia(Space))
                        .WithInitializer(EqualsValueClause(ConditionalExpression(
                            MemberAccessExpression(SimpleMemberAccessExpression,
                                IdentifierName(field.Name.AsParameter()), IdentifierName("HasValue")),
                            MemberAccessExpression(SimpleMemberAccessExpression,
                                IdentifierName(field.Name.AsParameter()), IdentifierName("Value")),
                            MemberAccessExpression(SimpleMemberAccessExpression, ThisExpression(),
                                IdentifierName(field.Name)))))))));

            var invocationExpressions = fields.Select((field, index) =>
                InvocationExpression(IdentifierName("ReferenceEquals")).WithArgumentList(ArgumentList(
                    SeparatedList<ArgumentSyntax>(new SyntaxNodeOrToken[]
                    {
                        Argument(IdentifierName($"__local{index}")), Token(CommaToken),
                        Argument(MemberAccessExpression(SimpleMemberAccessExpression,
                            ThisExpression(), IdentifierName(field.Name)))
                    }))));

            var ifStatement =
                IfStatement(
                    Condition(invocationExpressions.ToArray()),
                    ReturnStatement(ThisExpression().WithLeadingTrivia(Space)));

            var constructorArguments = fields.SelectMany((_, index) => new SyntaxNodeOrToken[] { Argument(IdentifierName($"__local{index}")), Token(CommaToken) }).ToList();
            constructorArguments.RemoveAt(constructorArguments.Count - 1);

            var returnStatement = ReturnStatement(ObjectCreationExpression(IdentifierName(@class.Name).WithLeadingTrivia(Space)).WithArgumentList(
                ArgumentList(SeparatedList<ArgumentSyntax>(
                    constructorArguments))).WithLeadingTrivia(Space));

            var block = new List<StatementSyntax>(locals) { ifStatement, returnStatement };

            return MethodDeclaration(@class.Type.WithTrailingTrivia(Space), "With")
                .AddModifiers(Token(PublicKeyword).WithTrailingTrivia(Space))
                .AddParameterListParameters(GetWithParameters(fields).ToArray())
                .WithBody(Block(block));

            ExpressionSyntax Condition(InvocationExpressionSyntax[] syntaxes)
            {
                switch (syntaxes.Length)
                {
                    case 1:
                        return syntaxes[0];
                    case 2:
                        return BinaryExpression(LogicalAndExpression, syntaxes[0], syntaxes[1]);
                    default:
                        return BinaryExpression(LogicalAndExpression, syntaxes[0], Condition(syntaxes.Skip(1).ToArray()));
                }
            }
        }

        static IEnumerable<ParameterSyntax> GetWithParameters(IEnumerable<Field> fields)
            => fields.Select(GetWithParameter);

        static ParameterSyntax GetWithParameter(Field field)
            => Parameter(Identifier(field.Name.AsParameter()))
                .WithType(ParseTypeName($"Optional<{field.Type}>"))
                .WithDefault(
                    EqualsValueClause(
                        DefaultExpression(
                            IdentifierName($"Optional<{field.Type}>"))));
    }
}