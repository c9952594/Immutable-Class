using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Diagnostics;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using static Microsoft.CodeAnalysis.CSharp.SyntaxKind;

namespace ConsoleApplication1
{
    class TypeName
    {
        readonly string _value1;
        readonly string _value2;
        #region Generated code: Immutable class
        public string Value1 => _value1;
        public string Value2 => _value2;
        public TypeName(string value1, string value2) { _value1 = value1; _value2 = value2; }

        public TypeName With(Optional<string> value1 = default(Optional<string>),
            Optional<string> value2 = default(Optional<string>)) => null;
        public struct Optional<T>
        {
            public T Value {
                get;
            }

            public bool HasValue {
                get;
            }

            public Optional(T value)
            {
                Value = value;
                HasValue = true;
            }

            public static explicit operator T(Optional<T> optional) => optional.Value;
            public static implicit operator Optional<T>(T value) => new Optional<T>(value);
            public override string ToString() => $"Optional (HasValue: {HasValue}, Value: '{Value}')";
        }
        #endregion
    }

    class asd
    {
        void qwe()
        {
            CompilationUnit().WithMembers(SingletonList<MemberDeclarationSyntax>(
                MethodDeclaration(IdentifierName("TypeName"), Identifier("With"))
                    .WithModifiers(TokenList(Token(PublicKeyword)))
                    .WithParameterList(ParameterList(SeparatedList<ParameterSyntax>(new SyntaxNodeOrToken[]
                    {
                        Parameter(Identifier("value1"))
                            .WithType(GenericName(Identifier("Optional")).WithTypeArgumentList(
                                TypeArgumentList(
                                    SingletonSeparatedList<TypeSyntax>(
                                        PredefinedType(Token(StringKeyword)))))).WithDefault(
                                EqualsValueClause(DefaultExpression(GenericName(Identifier("Optional"))
                                    .WithTypeArgumentList(TypeArgumentList(
                                        SingletonSeparatedList<TypeSyntax>(
                                            PredefinedType(Token(StringKeyword)))))))),
                        Token(CommaToken),
                        Parameter(Identifier("value2"))
                            .WithType(GenericName(Identifier("Optional")).WithTypeArgumentList(
                                TypeArgumentList(
                                    SingletonSeparatedList<TypeSyntax>(
                                        PredefinedType(Token(StringKeyword)))))).WithDefault(
                                EqualsValueClause(DefaultExpression(GenericName(Identifier("Optional"))
                                    .WithTypeArgumentList(TypeArgumentList(
                                        SingletonSeparatedList<TypeSyntax>(
                                            PredefinedType(Token(StringKeyword))))))))
                    }))).WithBody(Block(
                        IfStatement(
                            BinaryExpression(LogicalAndExpression,
                                BinaryExpression(LogicalAndExpression,
                                    InvocationExpression(MemberAccessExpression(SimpleMemberAccessExpression,
                                        PredefinedType(Token(ObjectKeyword)),
                                        IdentifierName("ReferenceEquals"))).WithArgumentList(ArgumentList(
                                        SeparatedList<ArgumentSyntax>(new SyntaxNodeOrToken[]
                                        {
                                            Argument(IdentifierName("value1")), Token(CommaToken),
                                            Argument(MemberAccessExpression(SimpleMemberAccessExpression,
                                                ThisExpression(), IdentifierName("_value1")))
                                        }))),
                                    InvocationExpression(MemberAccessExpression(SimpleMemberAccessExpression,
                                        PredefinedType(Token(ObjectKeyword)),
                                        IdentifierName("ReferenceEquals"))).WithArgumentList(ArgumentList(
                                        SeparatedList<ArgumentSyntax>(new SyntaxNodeOrToken[]
                                        {
                                            Argument(IdentifierName("value2")), Token(CommaToken),
                                            Argument(MemberAccessExpression(SimpleMemberAccessExpression,
                                                ThisExpression(), IdentifierName("_value2")))
                                        })))),
                                InvocationExpression(MemberAccessExpression(SimpleMemberAccessExpression,
                                        PredefinedType(Token(ObjectKeyword)),
                                        IdentifierName("ReferenceEquals")))
                                    .WithArgumentList(ArgumentList(SeparatedList<ArgumentSyntax>(new SyntaxNodeOrToken[]
                                    {
                                        Argument(IdentifierName("value3")), Token(CommaToken),
                                        Argument(MemberAccessExpression(SimpleMemberAccessExpression,
                                            ThisExpression(), IdentifierName("_value3")))
                                    })))), ReturnStatement(ThisExpression())),
                        ReturnStatement(LiteralExpression(NullLiteralExpression)))))).NormalizeWhitespace();
        }
    }
}