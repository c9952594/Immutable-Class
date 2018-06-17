using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace ImmutableClass
{
    internal class Field
    {
        public readonly string Name;
        public readonly TypeSyntax Type;

        public Field(TypeSyntax type, string valueText)
        {
            Type = type;
            Name = valueText;
        }
    }
}