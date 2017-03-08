using System;
using System.Collections.Generic;
using System.Composition;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.Rename;
using Microsoft.CodeAnalysis.Text;
using ClassDeclarationSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.ClassDeclarationSyntax;
using ExpressionStatementSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.ExpressionStatementSyntax;
using FieldDeclarationSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.FieldDeclarationSyntax;
using SyntaxKind = Microsoft.CodeAnalysis.CSharp.SyntaxKind;
using TypeDeclarationSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.TypeDeclarationSyntax;

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
                if (!(localDeclaration.Parent?.Parent is MethodDeclarationSyntax))
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

        private async  Task<Document> InjectInConstructorAndSetField(Document document, LocalDeclarationStatementSyntax localDeclaration, CancellationToken token)
        {
            
            var fieldName = localDeclaration.Declaration.Variables.ToString().Trim();
            var paramName = fieldName.Trim('_');
            var dependencyType = localDeclaration.Declaration.Type;

            if (token.IsCancellationRequested)
                return document;
            var oldRoot = await document.GetSyntaxRootAsync(token).ConfigureAwait(false);


            var oldClass = localDeclaration.Ancestors().OfType<ClassDeclarationSyntax>().First();

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


            var oldClassWithField = oldClass.WithMembers(oldClass.Members.Insert(lastIndex, fieldDeclaration));

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


                newclass = oldClass.WithMembers(oldClassWithField.Members.Insert(lastIndex+1, constructor));
            }

            var newRoot = oldRoot.ReplaceNode(oldClass, newclass);
            var newDoc = document.WithSyntaxRoot(newRoot);
            return newDoc;
        }

        private async Task<Solution> ReverseTypeNameAsync(Document document, TypeDeclarationSyntax typeDecl, CancellationToken cancellationToken)
        {
            // Produce a reversed version of the type declaration's identifier token.
            var identifierToken = typeDecl.Identifier;
            var newName = new string(identifierToken.Text.ToCharArray().Reverse().ToArray());

            // Get the symbol representing the type to be renamed.
            var semanticModel = await document.GetSemanticModelAsync(cancellationToken);
            var typeSymbol = semanticModel.GetDeclaredSymbol(typeDecl, cancellationToken);

            // Produce a new solution that has all references to that type renamed, including the declaration.
            var originalSolution = document.Project.Solution;
            var optionSet = originalSolution.Workspace.Options;
            var newSolution = await Renamer.RenameSymbolAsync(document.Project.Solution, typeSymbol, newName, optionSet, cancellationToken).ConfigureAwait(false);

            // Return the new solution with the now-uppercase type name.
            return newSolution;
        }
    }
}