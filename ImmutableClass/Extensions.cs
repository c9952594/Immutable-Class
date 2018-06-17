using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace ImmutableClass
{
    internal static class Extensions
    {
        public static string AsProperty(this string name)
        {
            var propertyName = name.Trim('_');
            propertyName = char.ToUpper(propertyName[0]) + propertyName.Substring(1);
            return propertyName;
        }

        public static string AsParameter(this string name)
        {
            return name.Trim('_');
        }
    }
}