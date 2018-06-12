using System.Collections.Generic;
using System.Collections.Immutable;
using System.Collections.ObjectModel;
using System.Composition;
using System.Linq;
using System.Linq.Expressions;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;

namespace ImmutableClass
{
    public static class Extensions
    {
        public static string AsProperty(this string value)
        {
            var propertyName = value.Trim('_');
            propertyName = char.ToUpper(propertyName[0]) + propertyName.Substring(1);
            return propertyName;
        }
    }

    class FieldMetadata
    {
        public readonly TypeSyntax Type;
        public readonly string Name;

        public FieldMetadata(TypeSyntax type, string name)
        {
            Type = type;
            Name = name;
        }

        public SyntaxToken Property => SyntaxFactory.Identifier(Name.AsProperty());
        public IdentifierNameSyntax Variable => SyntaxFactory.IdentifierName(Name);
    }

    class ClassMetadata
    {
        readonly TypeDeclarationSyntax _typeDecl;

        public ImmutableList<IEnumerable<FieldMetadata>> ReadonlyFields;

        public string Name => _typeDecl.Identifier.ValueText;
        public TypeSyntax Type => SyntaxFactory.ParseTypeName(Name);

        public ClassMetadata(TypeDeclarationSyntax typeDecl)
        {
            _typeDecl = typeDecl;

            ReadonlyFields = typeDecl.DescendantNodes().OfType<FieldDeclarationSyntax>()
                .Where(field => field.Modifiers.Any(modifier => modifier.Kind() == SyntaxKind.ReadOnlyKeyword))
                .Select(field => field.Declaration.Variables.Select(variable => new FieldMetadata(field.Declaration.Type, variable.Identifier.ValueText))).ToImmutableList();
        }
    }

    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(ImmutableClassCodeFixProvider)), Shared]
    public class ImmutableClassCodeFixProvider : CodeFixProvider
    {
        const string Title = "As Immutable Class";

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
            var metadata = new ClassMetadata(typeDecl);

            var documentEditor = await DocumentEditor.CreateAsync(document, cancellationToken);
            
            var properties = AddProperties(metadata);
            var constructor = AddConstructor(typeDecl, metadata);
            var methods = AddWithMethods(typeDecl, metadata);

            documentEditor.InsertAfter(metadata.ReadonlyFields.Last(), properties);
            documentEditor.InsertAfter(readonlyFields.Last(), constructor);
            documentEditor.InsertAfter(readonlyFields.Last(), methods);

            return document.Project.Solution.WithDocumentText(document.Id, await documentEditor.GetChangedDocument().GetTextAsync(cancellationToken));
        }

        static SeparatedSyntaxList<PropertyDeclarationSyntax> AddProperties(ClassMetadata @classMetadata)
        {
            return SyntaxFactory.SeparatedList(classMetadata.ReadonlyFields.SelectMany(variables =>
                variables.Select(variable =>
                    SyntaxFactory.PropertyDeclaration(variable.Type, variable.Property)
                        .AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword).WithTrailingTrivia(SyntaxFactory.Space))
                        .WithExpressionBody(SyntaxFactory.ArrowExpressionClause(variable.Variable))
                        .WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken))
                        .WithTrailingTrivia(SyntaxFactory.Whitespace("\n")))));
        }

        static ConstructorDeclarationSyntax AddConstructor(BaseTypeDeclarationSyntax typeDecl, FieldDeclarationSyntax[] readonlyFields)
        {
            return SyntaxFactory.ConstructorDeclaration(typeDecl.Identifier.ValueText)
                .AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword).WithTrailingTrivia(SyntaxFactory.Space))
                .AddParameterListParameters(
                    readonlyFields.SelectMany(field =>
                        field.Declaration.Variables.Select(variable =>
                            SyntaxFactory.Parameter(SyntaxFactory.Identifier(variable.Identifier.ValueText.Trim('_')))
                                .WithType(field.Declaration.Type))).ToArray())
                .WithBody(
                    SyntaxFactory.Block(
                        readonlyFields.SelectMany(field => 
                            field.Declaration.Variables.Select(variable =>
                                SyntaxFactory.ExpressionStatement(
                                    SyntaxFactory.AssignmentExpression(
                                        SyntaxKind.SimpleAssignmentExpression,
                                        SyntaxFactory.IdentifierName($"this.{variable.Identifier.ValueText}"),
                                        SyntaxFactory.IdentifierName(variable.Identifier.ValueText.Trim('_'))))))))
                .WithTrailingTrivia(SyntaxFactory.Whitespace("\n"));
        }

        static SeparatedSyntaxList<MethodDeclarationSyntax> AddWithMethods(BaseTypeDeclarationSyntax typeDecl, FieldDeclarationSyntax[] readonlyFields)
        {
            return SyntaxFactory.SeparatedList(readonlyFields.SelectMany(field =>
            {
                return field.Declaration.Variables.Select(variable =>
                {
                    var methodName = variable.Identifier.ValueText.Trim('_');
                    methodName = $"With{char.ToUpper(methodName[0]) + methodName.Substring(1)}";

                    var returnType = SyntaxFactory.ParseTypeName(typeDecl.Identifier.ValueText).WithTrailingTrivia(SyntaxFactory.Space);
                    var parameter = SyntaxFactory.Parameter(SyntaxFactory.Identifier("value")).WithType(field.Declaration.Type);

                    return SyntaxFactory.MethodDeclaration(returnType, methodName)
                        .AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword).WithTrailingTrivia(SyntaxFactory.Space))
                        .AddParameterListParameters(parameter)
                        .WithBody(
                            SyntaxFactory.Block(
                                SyntaxFactory.IfStatement(
                                    SyntaxFactory.InvocationExpression(
                                        SyntaxFactory.IdentifierName("ReferenceEquals"),
                                        SyntaxFactory.ArgumentList(SyntaxFactory.SeparatedList(new[]
                                        {
                                            SyntaxFactory.Argument(
                                                SyntaxFactory.IdentifierName("value")),
                                            SyntaxFactory.Argument(
                                                SyntaxFactory.IdentifierName("this." + variable.Identifier.ValueText))
                                        }))),

                            SyntaxFactory.Block(
                                SyntaxFactory.ReturnStatement(
                                    SyntaxFactory.ThisExpression()
                                                 .WithLeadingTrivia(SyntaxFactory.Space))),

                            SyntaxFactory.ElseClause(
                                SyntaxFactory.Block(
                                    SyntaxFactory.ReturnStatement(
                                        SyntaxFactory.ObjectCreationExpression(
                                            SyntaxFactory.ParseTypeName(typeDecl.Identifier.ValueText).WithLeadingTrivia(SyntaxFactory.Space),
                                            SyntaxFactory.ArgumentList(SyntaxFactory.SeparatedList(
                                                readonlyFields.SelectMany(_field =>
                                                    _field.Declaration.Variables.Select(_variable =>
                                                    {
                                                        var identifier = (_variable == variable) ? "value" : "this." + _variable.Identifier.ValueText;
                                                        return SyntaxFactory.Argument(
                                                            SyntaxFactory.IdentifierName(identifier));
                                                    })))),
                                            null
                                        ).WithLeadingTrivia(SyntaxFactory.Space)))))))
                        .WithTrailingTrivia(SyntaxFactory.Whitespace("\n"));
                });
            }));
        }
    }
}
