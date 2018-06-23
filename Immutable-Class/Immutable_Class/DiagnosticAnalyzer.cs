using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace Immutable_Class
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class ImmutableClassAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "ImmutableClass";
        static readonly LocalizableString Title = new LocalizableResourceString(nameof(Resources.AnalyzerTitle), Resources.ResourceManager, typeof(Resources));
        static readonly LocalizableString MessageFormat = new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));
        static readonly LocalizableString Description = new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager, typeof(Resources));
        const string Category = "Naming";
        const bool IsEnabledByDefault = true;

        static readonly DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, IsEnabledByDefault, Description);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSymbolAction(AnalyzeSymbol, SymbolKind.NamedType);
        }

        static void AnalyzeSymbol(SymbolAnalysisContext context)
        {
            var namedTypeSymbol = (INamedTypeSymbol)context.Symbol;

            var readonlyFields = namedTypeSymbol.DeclaringSyntaxReferences.First().GetSyntax()
                .DescendantNodes().OfType<FieldDeclarationSyntax>()
                .Where(field => field.Modifiers.Any(modifier => modifier.Kind() == SyntaxKind.ReadOnlyKeyword));

            if (readonlyFields.Any() == false) return;

            context.ReportDiagnostic(Diagnostic.Create(Rule, namedTypeSymbol.Locations[0], namedTypeSymbol.Name));
        }
    }
}
