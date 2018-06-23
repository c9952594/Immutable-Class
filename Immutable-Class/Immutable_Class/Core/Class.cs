using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace ImmutableClass
{
    internal class Class
    {
        public readonly string Name;
        public readonly TypeSyntax Type;

        public Class(string name)
        {
            Type = SyntaxFactory.ParseTypeName(name);
            Name = name;
        }
    }
}