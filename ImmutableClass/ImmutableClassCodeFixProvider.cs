using System.Collections.Generic;
using System.Collections.Immutable;
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
                    createChangedSolution: c => MakeUppercaseAsync(context.Document, declaration, c), 
                    equivalenceKey: Title),
                diagnostic);
        }

        static async Task<Solution> MakeUppercaseAsync(Document document, TypeDeclarationSyntax typeDecl, CancellationToken cancellationToken)
        {
            var fields = typeDecl.DescendantNodes().OfType<FieldDeclarationSyntax>();
            var readonlyFields = fields.Where(field => field.Modifiers.Any(modifier => modifier.Kind() == SyntaxKind.ReadOnlyKeyword)).ToArray();

            var documentEditor = await DocumentEditor.CreateAsync(document, cancellationToken);

            AddProperties(readonlyFields, documentEditor);
            AddConstructor(typeDecl, readonlyFields, documentEditor);
            AddWithMethods(typeDecl, readonlyFields, documentEditor);

            return document.Project.Solution.WithDocumentText(document.Id, await documentEditor.GetChangedDocument().GetTextAsync(cancellationToken));
        }

        static void AddProperties(FieldDeclarationSyntax[] readonlyFields, SyntaxEditor documentEditor)
        {
            var propertiesFromReadonlyFields = readonlyFields.SelectMany(field =>
            {
                var fieldType = field.Declaration.Type;
                return field.Declaration.Variables.Select(variable =>
                {
                    var variableName = variable.Identifier.ValueText;
                    var propertyName = variableName.Trim('_');
                    propertyName = char.ToUpper(propertyName[0]) + propertyName.Substring(1);

                    return SyntaxFactory.PropertyDeclaration(fieldType, SyntaxFactory.Identifier(propertyName))
                        .AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword).WithTrailingTrivia(SyntaxFactory.Space))
                        .WithExpressionBody(SyntaxFactory.ArrowExpressionClause(SyntaxFactory.IdentifierName(variableName)))
                        .WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken))
                        .WithTrailingTrivia(SyntaxFactory.Whitespace("\n"));
                });
            });

            documentEditor.InsertAfter(readonlyFields.Last(), propertiesFromReadonlyFields);
        }

        static void AddConstructor(BaseTypeDeclarationSyntax typeDecl, FieldDeclarationSyntax[] readonlyFields, SyntaxEditor documentEditor)
        {
            var constructorParameters = readonlyFields.SelectMany(field => 
                field.Declaration.Variables.Select(variable => 
                    SyntaxFactory.Parameter(SyntaxFactory.Identifier(variable.Identifier.ValueText.Trim('_'))).WithType(field.Declaration.Type)));

            var fieldAssignments = readonlyFields.SelectMany(field => field.Declaration.Variables.Select(variable =>
            {
                var fieldName = SyntaxFactory.IdentifierName($"this.{variable.Identifier.ValueText}");
                var parameterName = SyntaxFactory.IdentifierName(variable.Identifier.ValueText.Trim('_'));
                return SyntaxFactory.ExpressionStatement(SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, fieldName, parameterName));
            }));

            var constructor = SyntaxFactory.ConstructorDeclaration(typeDecl.Identifier.ValueText)
                                           .AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword).WithTrailingTrivia(SyntaxFactory.Space))
                                           .AddParameterListParameters(constructorParameters.ToArray())
                                           .WithBody(SyntaxFactory.Block(fieldAssignments))
                                           .WithTrailingTrivia(SyntaxFactory.Whitespace("\n"));

            documentEditor.InsertAfter(readonlyFields.Last(), constructor);
        }

        static void AddWithMethods(BaseTypeDeclarationSyntax typeDecl, FieldDeclarationSyntax[] readonlyFields, SyntaxEditor documentEditor)
        {
            var withMethods = readonlyFields.SelectMany(field =>
            {
                var returnType = SyntaxFactory.ParseTypeName(typeDecl.Identifier.ValueText).WithTrailingTrivia(SyntaxFactory.Space);

                return field.Declaration.Variables.Select(variable =>
                {
                    var methodName = variable.Identifier.ValueText.Trim('_');
                    methodName = $"With{char.ToUpper(methodName[0]) + methodName.Substring(1)}";

                    var parameter = SyntaxFactory.Parameter(SyntaxFactory.Identifier("value")).WithType(field.Declaration.Type);

                    var constructorParameters = SyntaxFactory.ArgumentList(SyntaxFactory.SeparatedList(
                        readonlyFields.SelectMany(_field =>
                            _field.Declaration.Variables.Select(_variable =>
                            {
                                if (_variable == variable)
                                {
                                    return SyntaxFactory.Argument(
                                        SyntaxFactory.IdentifierName("value"));
                                }
                                else
                                {
                                    return SyntaxFactory.Argument(
                                        SyntaxFactory.IdentifierName("this." + _variable.Identifier.ValueText));
                                }
                            }))));

                    var elseClauseSyntax =
                    SyntaxFactory.ElseClause(
                        SyntaxFactory.Block(
                            SyntaxFactory.ReturnStatement(
                                SyntaxFactory.ObjectCreationExpression(
                                    SyntaxFactory.ParseTypeName(typeDecl.Identifier.ValueText).WithLeadingTrivia(SyntaxFactory.Space),
                                    constructorParameters, 
                                    null
                                ).WithLeadingTrivia(SyntaxFactory.Space))));

                    var ifStatement = SyntaxFactory.IfStatement(
                        SyntaxFactory.InvocationExpression(
                            SyntaxFactory.IdentifierName("ReferenceEquals"),
                            SyntaxFactory.ArgumentList(SyntaxFactory.SeparatedList(new[]
                            {
                                SyntaxFactory.Argument(SyntaxFactory.IdentifierName("value")),
                                SyntaxFactory.Argument(
                                    SyntaxFactory.IdentifierName("this." + variable.Identifier.ValueText))
                            }))),

                        SyntaxFactory.Block(
                            SyntaxFactory.ReturnStatement(
                                SyntaxFactory.ThisExpression().WithLeadingTrivia(SyntaxFactory.Space))),

                        elseClauseSyntax);

                    return SyntaxFactory.MethodDeclaration(returnType, methodName)
                        .AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword).WithTrailingTrivia(SyntaxFactory.Space))
                        .AddParameterListParameters(parameter)
                        .WithBody(SyntaxFactory.Block(ifStatement))
                        .WithTrailingTrivia(SyntaxFactory.Whitespace("\n"));
                });
            });

            documentEditor.InsertAfter(readonlyFields.Last(), withMethods);
        }
    }
}
