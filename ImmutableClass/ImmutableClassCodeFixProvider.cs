using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CSharp;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using static Microsoft.CodeAnalysis.CSharp.SyntaxKind;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;
using System.Collections.Generic;


namespace ImmutableClass
{
    static class Extensions
    {
        public static string AsProperty(this string name) {
            var propertyName = name.Trim('_');
            propertyName = char.ToUpper(propertyName[0]) + propertyName.Substring(1);
            return propertyName;
        }

        public static string AsParameter(this string name)
        {
            return name.Trim('_');
        }
    }

    internal class Field
    {
        public readonly TypeSyntax Type;
        public readonly string Name;

        public Field(TypeSyntax type, string valueText)
        {
            Type = type;
            Name = valueText;
        }
    }

    internal class Class
    {
        public readonly TypeSyntax Type;
        public readonly string Name;

        public Class(string name)
        {
            Type = ParseTypeName(name);
            Name = name;
        }
    }

    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(ImmutableClassCodeFixProvider)), Shared]
    public class ImmutableClassCodeFixProvider : CodeFixProvider
    {
        const string Title = "As immutable class";

        public sealed override ImmutableArray<string> FixableDiagnosticIds => ImmutableArray.Create(ImmutableClassAnalyzer.DiagnosticId);

        public sealed override FixAllProvider GetFixAllProvider() => WellKnownFixAllProviders.BatchFixer;

        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            var diagnostic = context.Diagnostics.First();
            var diagnosticSpan = diagnostic.Location.SourceSpan;

            var declaration = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType<TypeDeclarationSyntax>().First();

            context.RegisterCodeFix(
                CodeAction.Create(
                    title: Title,
                    createChangedSolution: c => MakeImmutableAsync(context.Document, declaration, c), 
                    equivalenceKey: Title),
                diagnostic);
        }

        static async Task<Solution> MakeImmutableAsync(Document document, TypeDeclarationSyntax typeDecl, CancellationToken cancellationToken)
        {
            var fieldDeclarations = typeDecl.DescendantNodes().OfType<FieldDeclarationSyntax>();
            var readonlyFieldDeclarations = fieldDeclarations.Where(field => field.Modifiers.Any(modifier => modifier.Kind() == ReadOnlyKeyword)).ToArray();

            var @class = new Class(typeDecl.Identifier.ValueText);

            var fields = readonlyFieldDeclarations.SelectMany(field => 
                field.Declaration.Variables.Select(variable => 
                    new Field(field.Declaration.Type, variable.Identifier.ValueText))).ToArray();
            
            var documentEditor = await DocumentEditor.CreateAsync(document, cancellationToken);

            var properties = GetProperties(fields).ToArray();
            var constructor = GetConstructor(@class, fields);
            //var methods = AddWithMethods(typeDecl, readonlyFieldDeclarations);
            
            var surroundedByRegion = SurroundWithRegion(properties, constructor);
            documentEditor.InsertAfter(readonlyFieldDeclarations.Last(), surroundedByRegion);

            return document.Project.Solution.WithDocumentText(document.Id, await documentEditor.GetChangedDocument().GetTextAsync(cancellationToken));
        }

        private static IEnumerable<MemberDeclarationSyntax> SurroundWithRegion(PropertyDeclarationSyntax[] properties, ConstructorDeclarationSyntax constructor)
        {
            const string regionText = @"Generated code: Immutable class";

            var all = List<MemberDeclarationSyntax>(properties).Add(constructor).ToArray();

            all[0] = all[0].WithLeadingTrivia(
                Trivia(RegionDirectiveTrivia(true)
                    .WithHashToken(Token(HashToken))
                    .WithRegionKeyword(Token(RegionKeyword)).WithEndOfDirectiveToken(
                        Token(
                            TriviaList(Space, PreprocessingMessage(regionText)),
                            EndOfDirectiveToken,
                            TriviaList()))),
                CarriageReturnLineFeed
            );

            all[all.Length - 1] = all[all.Length - 1].WithTrailingTrivia(
                Trivia(
                    EndRegionDirectiveTrivia(true).WithLeadingTrivia(CarriageReturnLineFeed)
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
                .WithTrailingTrivia(Whitespace("\n"));

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

        static ReturnStatementSyntax ReturnThis()
            => ReturnStatement(ThisExpression().WithLeadingTrivia(Space));
        
        static SeparatedSyntaxList<MethodDeclarationSyntax> AddWithMethods(BaseTypeDeclarationSyntax typeDecl, FieldDeclarationSyntax[] readonlyFields)
        {
            return SeparatedList(readonlyFields.SelectMany(field =>
            {
                return field.Declaration.Variables.Select(variable =>
                {
                    var methodName = variable.Identifier.ValueText.Trim('_');
                    methodName = $"With{char.ToUpper(methodName[0]) + methodName.Substring(1)}";

                    var returnType = ParseTypeName(typeDecl.Identifier.ValueText).WithTrailingTrivia(Space);
                    var parameter = Parameter(Identifier("value")).WithType(field.Declaration.Type);

                    return MethodDeclaration(returnType, methodName)
                        .AddModifiers(Token(PublicKeyword).WithTrailingTrivia(Space))
                        .AddParameterListParameters(parameter)
                        .WithBody(
                            Block(
                                IfStatement(
                                    InvocationExpression(
                                        IdentifierName("ReferenceEquals"),
                                        ArgumentList(SeparatedList(new[]
                                        {
                                            Argument(
                                                IdentifierName("value")),
                                            Argument(
                                                IdentifierName("this." + variable.Identifier.ValueText))
                                        }))),
                                    
                            ReturnThis(),

                            ElseClause(
                                ReturnStatement(
                                    ObjectCreationExpression(
                                        ParseTypeName(typeDecl.Identifier.ValueText).WithLeadingTrivia(Space),
                                        ArgumentList(SeparatedList(
                                            readonlyFields.SelectMany(_field =>
                                                _field.Declaration.Variables.Select(_variable =>
                                                {
                                                    var identifier = (_variable == variable) ? "value" : "this." + _variable.Identifier.ValueText;
                                                    return Argument(
                                                        IdentifierName(identifier));
                                                })))),
                                        null
                                    ).WithLeadingTrivia(Space))))))
                        .WithTrailingTrivia(CarriageReturnLineFeed);
                });
            }));
        }
    }

    
}
