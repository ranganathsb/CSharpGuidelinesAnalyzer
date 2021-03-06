using System.Collections.Immutable;
using System.Linq;
using CSharpGuidelinesAnalyzer.Extensions;
using JetBrains.Annotations;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;

namespace CSharpGuidelinesAnalyzer.Rules.MemberDesign
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public sealed class MembersShouldDoASingleThingAnalyzer : GuidelineAnalyzer
    {
        public const string DiagnosticId = "AV1115";

        private const string Title = "Member contains the word 'and'";
        private const string MessageFormat = "{0} '{1}' contains the word 'and'.";
        private const string Description = "A method or property should do only one thing.";

        [NotNull]
        private static readonly AnalyzerCategory Category = AnalyzerCategory.MemberDesign;

        [NotNull]
        private static readonly DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat,
            Category.Name, DiagnosticSeverity.Warning, true, Description, Category.HelpLinkUri);

        [ItemNotNull]
        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

        private static readonly ImmutableArray<SymbolKind> MemberSymbolKinds =
            ImmutableArray.Create(SymbolKind.Property, SymbolKind.Method, SymbolKind.Field, SymbolKind.Event);

        [ItemNotNull]
        private static readonly ImmutableArray<string> WordsBlacklist = ImmutableArray.Create("and");

        public override void Initialize([NotNull] AnalysisContext context)
        {
            context.EnableConcurrentExecution();
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);

            context.RegisterSymbolAction(c => c.SkipEmptyName(AnalyzeMember), MemberSymbolKinds);
        }

        private void AnalyzeMember(SymbolAnalysisContext context)
        {
            if (context.Symbol.IsPropertyOrEventAccessor() || context.Symbol.IsUnitTestMethod())
            {
                return;
            }

            if (ContainsBlacklistedWord(context.Symbol.Name))
            {
                context.ReportDiagnostic(Diagnostic.Create(Rule, context.Symbol.Locations[0], context.Symbol.Kind,
                    context.Symbol.Name));
            }
        }

        private static bool ContainsBlacklistedWord([NotNull] string name)
        {
            return name.GetWordsInList(WordsBlacklist).Any();
        }
    }
}
