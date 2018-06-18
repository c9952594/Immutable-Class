using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Diagnostics;

namespace ConsoleApplication1
{
    class TypeName
    {
        readonly string _value1;
        readonly string _value2;
        readonly StringBuilder _value3;

        #region Generated code: Immutable class

        public string Value1 => _value1;
        public string Value2 => _value2;
        public StringBuilder Value3 => _value3;

        public TypeName(string value1, string value2, StringBuilder value3)
        {
            this._value1 = value1;
            this._value2 = value2;
            this._value3 = value3;
        }

        public TypeName With(Optional<string> value1 = default(Optional<string>),
            Optional<string> value2 = default(Optional<string>),
            Optional<StringBuilder> value3 = default(Optional<StringBuilder>))
        {
            var __local0 = value1.HasValue ? value1.Value : this._value1;
            var __local1 = value2.HasValue ? value2.Value : this._value2;
            var __local2 = value3.HasValue ? value3.Value : this._value3;
            if (ReferenceEquals(__local0, this._value1) && ReferenceEquals(__local1, this._value2) &&
                ReferenceEquals(__local2, this._value3)) return this;
            return new TypeName(__local0, __local1, __local2);
        }

        public struct Optional<T>
        {
            public T Value { get; }

            public bool HasValue { get; }

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
}