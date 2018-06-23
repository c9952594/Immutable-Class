using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using ApprovalTests;
using ApprovalTests.Namers;
using ApprovalTests.Reporters;
using ApprovalTests.Writers;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.Diagnostics;
using NUnit.Framework;
using TestHelper;

namespace Immutable_Class.Test
{
    public class Fixes : CodeFixVerifier
    {
        static IEnumerable<TestCaseData> _classFiles = 
            Directory.GetFiles(
                Path.Combine(
                    TestContext.CurrentContext.TestDirectory,
                    "TestData"
                )
            )
            .Select(file => new TestCaseData(file).SetName(Path.GetFileNameWithoutExtension(file)));

        [Test, 
         TestCaseSource(nameof(_classFiles)), 
         UseReporter(typeof(BeyondCompare4Reporter)), 
         UseApprovalSubdirectory("TestData")]
        public void Fix(string filename)
        {
            var original = File.ReadAllText(filename);
            var altered = VerifyCSharpFix(original);
            var writer = new CompilationWriter(altered, Path.GetFileNameWithoutExtension(filename));
            Approvals.Verify(writer);
        }

        protected override CodeFixProvider GetCSharpCodeFixProvider() => new ImmutableClassCodeFixProvider();
        protected override DiagnosticAnalyzer GetCSharpDiagnosticAnalyzer() => new ImmutableClassAnalyzer();

        class CompilationWriter : ApprovalTextWriter
        {
            readonly string _filename;

            public CompilationWriter(string data, string filename) : base(data, "cs") => _filename = filename;

            public override string GetApprovalFilename(string basename)
            {
                var approvalFilename = Path.Combine(Path.GetDirectoryName(basename) ?? throw new InvalidOperationException(), $"{_filename}{WriterUtils.Approved}{ExtensionWithDot}");
                if (File.Exists(approvalFilename) == false) File.Create(approvalFilename).Close();
                return approvalFilename;
            }

            public override string GetReceivedFilename(string basename)
            {
                var receivedFilename = Path.Combine(Path.GetDirectoryName(basename) ?? throw new InvalidOperationException(), $"{_filename}{WriterUtils.Received}{ExtensionWithDot}");
                if (File.Exists(receivedFilename) == false) File.Create(receivedFilename).Close();
                return receivedFilename;
            }
        }
    }
}
