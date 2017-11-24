using System;
using System.Linq;
using System.Threading;
using JetBrains.Annotations;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Operations;
using Microsoft.CodeAnalysis.Text;

namespace CSharpGuidelinesAnalyzer.Extensions
{
    /// <summary />
    internal static class OperationExtensions
    {
        [CanBeNull]
        public static IdentifierInfo TryGetIdentifierInfo([CanBeNull] this IOperation identifier)
        {
            var visitor = new IdentifierVisitor();
            return visitor.Visit(identifier, null);
        }

        private sealed class IdentifierVisitor : OperationVisitor<object, IdentifierInfo>
        {
            [NotNull]
            public override IdentifierInfo VisitLocalReference([NotNull] ILocalReferenceOperation operation, [CanBeNull] object argument)
            {
                var name = new IdentifierName(operation.Local.Name,
                    operation.Local.ToDisplayString(SymbolDisplayFormat.CSharpShortErrorMessageFormat));
                return new IdentifierInfo(name, operation.Local.Type, "Variable");
            }

            [NotNull]
            public override IdentifierInfo VisitParameterReference([NotNull] IParameterReferenceOperation operation, [CanBeNull] object argument)
            {
                var name = new IdentifierName(operation.Parameter.Name,
#pragma warning disable AV2310 // Code blocks should not contain inline comments
                    /* CSharpShortErrorMessageFormat returns 'ref int', ie. without parameter name */
#pragma warning restore AV2310 // Code blocks should not contain inline comments
                    operation.Parameter.Name);
                return new IdentifierInfo(name, operation.Parameter.Type, operation.Parameter.Kind.ToString());
            }

            [NotNull]
            public override IdentifierInfo VisitFieldReference([NotNull] IFieldReferenceOperation operation, [CanBeNull] object argument)
            {
                return CreateForMemberReferenceExpression(operation, operation.Field.Type);
            }

            [NotNull]
            public override IdentifierInfo VisitEventReference([NotNull] IEventReferenceOperation operation, [CanBeNull] object argument)
            {
                return CreateForMemberReferenceExpression(operation, operation.Event.Type);
            }

            [NotNull]
            public override IdentifierInfo VisitPropertyReference([NotNull] IPropertyReferenceOperation operation, [CanBeNull] object argument)
            {
                return CreateForMemberReferenceExpression(operation, operation.Property.Type);
            }

            [NotNull]
            private IdentifierInfo CreateForMemberReferenceExpression([NotNull] IMemberReferenceOperation operation,
                [NotNull] ITypeSymbol memberType)
            {
                var name = new IdentifierName(operation.Member.Name,
                    operation.Member.ToDisplayString(SymbolDisplayFormat.CSharpShortErrorMessageFormat));
                return new IdentifierInfo(name, memberType, operation.Member.Kind.ToString());
            }

            [NotNull]
            public override IdentifierInfo VisitInvocation([NotNull] IInvocationOperation operation, [CanBeNull] object argument)
            {
                var name = new IdentifierName(operation.TargetMethod.Name,
                    operation.TargetMethod.ToDisplayString(SymbolDisplayFormat.CSharpShortErrorMessageFormat));
                return new IdentifierInfo(name, operation.TargetMethod.ReturnType, operation.TargetMethod.Kind.ToString());
            }
        }

        [CanBeNull]
        public static Location GetLocationForKeyword([NotNull] this IOperation operation,
            LookupKeywordStrategy lookupStrategy = LookupKeywordStrategy.PreferDoKeywordInDoWhileLoop)
        {
            var visitor = new OperationLocationVisitor(lookupStrategy);
            return visitor.Visit(operation, null);
        }

        private sealed class OperationLocationVisitor : OperationVisitor<object, Location>
        {
            private readonly LookupKeywordStrategy lookupStrategy;

            public OperationLocationVisitor(LookupKeywordStrategy lookupStrategy)
            {
                this.lookupStrategy = lookupStrategy;
            }

            [NotNull]
            public override Location VisitWhileLoop([NotNull] IWhileLoopOperation operation, [CanBeNull] object argument)
            {
                SyntaxToken keyword;
                if (operation.ConditionIsTop)
                {
                    keyword = ((WhileStatementSyntax)operation.Syntax).WhileKeyword;
                }
                else
                {
                    var doSyntax = (DoStatementSyntax)operation.Syntax;
                    keyword = lookupStrategy == LookupKeywordStrategy.PreferDoKeywordInDoWhileLoop
                        ? doSyntax.DoKeyword
                        : doSyntax.WhileKeyword;
                }

                return keyword.GetLocation();
            }

            [NotNull]
            public override Location VisitForLoop([NotNull] IForLoopOperation operation, [CanBeNull] object argument)
            {
                var syntax = (ForStatementSyntax)operation.Syntax;
                return syntax.ForKeyword.GetLocation();
            }

            [NotNull]
            public override Location VisitForEachLoop([NotNull] IForEachLoopOperation operation, [CanBeNull] object argument)
            {
                var syntax = (ForEachStatementSyntax)operation.Syntax;
                return syntax.ForEachKeyword.GetLocation();
            }

            [NotNull]
            public override Location VisitReturn([NotNull] IReturnOperation operation, [CanBeNull] object argument)
            {
                return GetLocationForReturnOrYield(operation);
            }

            [NotNull]
            private static Location GetLocationForReturnOrYield([NotNull] IReturnOperation operation)
            {
                // TODO: Inline

                if (operation.Syntax is ReturnStatementSyntax returnSyntax)
                {
                    return returnSyntax.ReturnKeyword.GetLocation();
                }

                if (operation.Syntax is YieldStatementSyntax yieldSyntax)
                {
                    return GetLocationForYieldStatement(yieldSyntax);
                }

                throw ExceptionFactory.Unreachable();
            }

            [NotNull]
            private static Location GetLocationForYieldStatement([NotNull] YieldStatementSyntax yieldSyntax)
            {
                int start = yieldSyntax.YieldKeyword.GetLocation().SourceSpan.Start;
                int end = yieldSyntax.ReturnOrBreakKeyword.GetLocation().SourceSpan.End;
                TextSpan sourceSpan = TextSpan.FromBounds(start, end);

                return Location.Create(yieldSyntax.SyntaxTree, sourceSpan);
            }

            [NotNull]
            public override Location VisitConditional([NotNull] IConditionalOperation operation, [CanBeNull] object argument)
            {
                if (operation.IsStatement())
                {
                    var syntax = (IfStatementSyntax)operation.Syntax;
                    return syntax.IfKeyword.GetLocation();
                }

                return base.VisitConditional(operation, argument);
            }

            [NotNull]
            public override Location VisitUsing([NotNull] IUsingOperation operation, [CanBeNull] object argument)
            {
                var syntax = (UsingStatementSyntax)operation.Syntax;
                return syntax.UsingKeyword.GetLocation();
            }

            [NotNull]
            public override Location VisitLock([NotNull] ILockOperation operation, [CanBeNull] object argument)
            {
                var syntax = (LockStatementSyntax)operation.Syntax;
                return syntax.LockKeyword.GetLocation();
            }

            [NotNull]
            public override Location VisitSwitch([NotNull] ISwitchOperation operation, [CanBeNull] object argument)
            {
                var syntax = (SwitchStatementSyntax)operation.Syntax;
                return syntax.SwitchKeyword.GetLocation();
            }

            [NotNull]
            public override Location VisitThrow([NotNull] IThrowOperation operation, [CanBeNull] object argument)
            {
                if (operation.IsStatement())
                {
                    var syntax = (ThrowStatementSyntax)operation.Syntax;
                    return syntax.ThrowKeyword.GetLocation();
                }

                return base.VisitThrow(operation, argument);
            }

            [NotNull]
            public override Location VisitSingleValueCaseClause([NotNull] ISingleValueCaseClauseOperation operation, [CanBeNull] object argument)
            {
                var syntax = (SwitchLabelSyntax)operation.Syntax;
                return syntax.Keyword.GetLocation();
            }

            [NotNull]
            public override Location VisitDefaultCaseClause([NotNull] IDefaultCaseClauseOperation operation, [CanBeNull] object argument)
            {
                var syntax = (DefaultSwitchLabelSyntax)operation.Syntax;
                return syntax.Keyword.GetLocation();
            }

            [NotNull]
            public override Location VisitPatternCaseClause([NotNull] IPatternCaseClauseOperation operation, [CanBeNull] object argument)
            {
                // TODO
                throw new NotImplementedException();
            }
        }

        public static bool HasErrors([NotNull] this IOperation operation, [NotNull] Compilation compilation,
            CancellationToken cancellationToken = default(CancellationToken))
        {
            Guard.NotNull(operation, nameof(operation));
            Guard.NotNull(compilation, nameof(compilation));

            if (operation.Syntax == null)
            {
                return true;
            }

            SemanticModel model = compilation.GetSemanticModel(operation.Syntax.SyntaxTree);

            return model.GetDiagnostics(operation.Syntax.Span, cancellationToken)
                .Any(d => d.Severity == DiagnosticSeverity.Error);
        }

        public static bool IsStatement([NotNull] this IOperation operation)
        {
            if (operation.Syntax is ThrowExpressionSyntax)
            {
                return false;
            }
            if (operation is IVariableDeclarationGroupOperation declarationGroup)
            {
                if (!(declarationGroup.Parent is IBlockOperation))
                {
                    return false;
                }
            }
            if (operation is IExpressionStatementOperation expressionStatement)
            {
                if (!(expressionStatement.Parent is IBlockOperation))
                {
                    return false;
                }
            }

            return operation.Type == null;
        }
    }
}
