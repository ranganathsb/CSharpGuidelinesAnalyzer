using System.Collections.Immutable;
using System.Linq;
using CSharpGuidelinesAnalyzer.Extensions;
using JetBrains.Annotations;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Operations;

namespace CSharpGuidelinesAnalyzer.Rules.Framework
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public sealed class OnlyUseDynamicForUnknownTypesAnalyzer : GuidelineAnalyzer
    {
        public const string DiagnosticId = "AV2230";

        private const string Title = "A non-dynamic result is implicitly assigned to a dynamic identifier";
        private const string MessageFormat = "A non-dynamic result is implicitly assigned to dynamic identifier '{0}'.";
        private const string Description = "Only use the dynamic keyword when talking to a dynamic object.";
        private const string Category = "Framework";

        [NotNull]
        private static readonly DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category,
            DiagnosticSeverity.Warning, true, Description, HelpLinkUris.GetForCategory(Category, DiagnosticId));

        [ItemNotNull]
        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

        public override void Initialize([NotNull] AnalysisContext context)
        {
            context.EnableConcurrentExecution();
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);

            context.RegisterCompilationStartAction(startContext =>
            {
                if (startContext.Compilation.SupportsOperations())
                {
                    startContext.RegisterOperationAction(c => c.SkipInvalid(AnalyzeVariableDeclaration),
                        OperationKind.VariableDeclaration);

                    startContext.RegisterOperationAction(c => c.SkipInvalid(AnalyzeAssignment), OperationKind.SimpleAssignment);

                }
            });
        }

        private void AnalyzeVariableDeclaration(OperationAnalysisContext context)
        {
            var declaration = (IVariableDeclarationOperation)context.Operation;

            foreach (var declarator in declaration.Declarators)
            {
                ILocalSymbol variable = declarator.Symbol;

                if (IsDynamicType(variable.Type) && declarator.Initializer != null)
                {
                    if (RequiresReport(declarator.Initializer.Value))
                    {
                        context.ReportDiagnostic(Diagnostic.Create(Rule, declarator.Syntax.GetLocation(), variable.Name));
                    }
                }
            }
        }

        private static bool IsDynamicType([NotNull] ITypeSymbol type)
        {
            return type.TypeKind == TypeKind.Dynamic;
        }

        private void AnalyzeAssignment(OperationAnalysisContext context)
        {
            var assignment = (IAssignmentOperation)context.Operation;

            IdentifierInfo identifierInfo = assignment.Target.TryGetIdentifierInfo();
            if (identifierInfo != null && IsDynamicType(identifierInfo.Type))
            {
                if (RequiresReport(assignment.Value))
                {
                    context.ReportDiagnostic(Diagnostic.Create(Rule, assignment.Syntax.GetLocation(),
                        identifierInfo.Name.ShortName));
                }
            }
        }

        private bool RequiresReport([CanBeNull] IOperation value)
        {
            if (value is IConversionOperation conversion && conversion.IsImplicit)
            {
                var sourceType = conversion.Operand.Type;

                if (sourceType != null && !IsDynamicType(sourceType) && sourceType.SpecialType != SpecialType.System_Object)
                {
                    return true;
                }
            }

            return false;
        }
    }
}
