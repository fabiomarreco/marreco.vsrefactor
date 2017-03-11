using System.Collections.Generic;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Rename;
using Microsoft.CodeAnalysis.Text;
using Microsoft.CodeAnalysis.Formatting;

namespace Marreco.VSRefactor
{
    [ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(MarrecoVSRefactorCodeRefactoringProvider)), Shared]
    internal class MarrecoVSRefactorCodeRefactoringProvider : CodeRefactoringProvider
    {
        public sealed override async Task ComputeRefactoringsAsync(CodeRefactoringContext context)
        {
            // TODO: Replace the following code with your own analysis, generating a CodeAction for each refactoring to offer

            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            // Find the node at the selection.
            var node = root.FindNode(context.Span);

            // Only offer a refactoring if the selected node is a type declaration node.
            var localDeclaration = node as LocalDeclarationStatementSyntax;
            if (localDeclaration != null)
            {
                if (!(localDeclaration.Ancestors().OfType<MethodDeclarationSyntax>().Any()))
                    return;


                var text = localDeclaration.Declaration.Variables.ToString().Trim();
                if (!text.StartsWith("_"))
                    return;

                var classDeclr = localDeclaration.Ancestors().OfType<ClassDeclarationSyntax>().FirstOrDefault();
                if (classDeclr == null)
                    return;

                if (classDeclr.Members.OfType<FieldDeclarationSyntax>()
                    .Any<FieldDeclarationSyntax>(s => s.Declaration.Variables.Any(v => v.ToString() == text)))
                    return;


                //classDeclr.AddMembers(new MemberDeclarationSyntax())

                // classDeclr.Members.OfType<FieldDeclarationSyntax>().Any(f=> f.)
                var action = CodeAction.Create("Inject in constructor and set field", c => InjectInConstructorAndSetField(context.Document, localDeclaration, c));
                context.RegisterRefactoring(action);
            }

        }

        private async Task<Document> InjectInConstructorAndSetField(Document document, LocalDeclarationStatementSyntax localDeclaration, CancellationToken token)
        {

            var fieldName = localDeclaration.Declaration.Variables.ToString().Trim();
            var paramName = fieldName.Trim('_');
            var dependencyType = localDeclaration.Declaration.Type;

            if (token.IsCancellationRequested)
                return document;
            var oldRoot = await document.GetSyntaxRootAsync(token).ConfigureAwait(false);


            var oldClass = localDeclaration.Ancestors().OfType<ClassDeclarationSyntax>().First();

            var oldClassCodeRemoved = oldClass.RemoveNode(localDeclaration, SyntaxRemoveOptions.KeepTrailingTrivia);




            int lastIndex =
                oldClass.Members.Select((m, i) => new { i, m })
                    .SkipWhile(s => s.m is FieldDeclarationSyntax)
                    .Select(s => s.i)
                    .FirstOrDefault();

            var fieldDeclaration =
                SyntaxFactory.FieldDeclaration(localDeclaration.Declaration)
                .WithModifiers(
                    SyntaxFactory.TokenList(
                        SyntaxFactory.Token(SyntaxKind.PrivateKeyword),
                        SyntaxFactory.Token(SyntaxKind.ReadOnlyKeyword)));


            var oldClassWithField = oldClassCodeRemoved.WithMembers(oldClassCodeRemoved.Members.Insert(lastIndex, fieldDeclaration));

            var oldConstructor = oldClassWithField
                     .DescendantNodes()
                     .OfType<ConstructorDeclarationSyntax>()
                     .FirstOrDefault();

            ClassDeclarationSyntax newclass;

            if (oldConstructor != null)
            {
                var newConstructor = oldConstructor
                    .WithBody(oldConstructor.Body.AddStatements(
                        SyntaxFactory.ExpressionStatement(
                            SyntaxFactory.AssignmentExpression(
                                SyntaxKind.SimpleAssignmentExpression,
                                SyntaxFactory.IdentifierName(fieldName),
                                SyntaxFactory.IdentifierName(paramName)))));

                newConstructor = newConstructor
                    .WithParameterList(oldConstructor.ParameterList.AddParameters(
                        SyntaxFactory.Parameter(
                            SyntaxFactory.Identifier(paramName)).WithType(dependencyType)))
                            .WithAdditionalAnnotations(Formatter.Annotation);


                newclass = oldClassWithField.ReplaceNode(oldConstructor, newConstructor);
            }
            else
            {
                var paramList = SyntaxFactory.ParameterList(
                    SyntaxFactory.SeparatedList<ParameterSyntax>(new ParameterSyntax[] { SyntaxFactory.Parameter(
                        SyntaxFactory.Identifier(paramName)).WithType(dependencyType) }));

                var constructor =
                    SyntaxFactory.ConstructorDeclaration(oldClassWithField.Identifier.Text)
                        .WithParameterList(paramList)
                        .WithModifiers(SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PublicKeyword)))
                        .WithBody(SyntaxFactory.Block(
                        SyntaxFactory.ExpressionStatement(
                            SyntaxFactory.AssignmentExpression(
                                SyntaxKind.SimpleAssignmentExpression,
                                SyntaxFactory.IdentifierName(fieldName),
                                SyntaxFactory.IdentifierName(paramName)))));


                newclass = oldClassCodeRemoved.WithMembers(oldClassWithField.Members.Insert(lastIndex + 1, constructor));
            }
            var newRoot = oldRoot.ReplaceNode(oldClass, newclass);
            var newDoc = document.WithSyntaxRoot(newRoot);
            return newDoc;
        }
    }
}