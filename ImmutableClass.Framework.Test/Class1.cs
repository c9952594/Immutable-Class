using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Alphaleonis.Win32.Filesystem;
using ApprovalTests;
using ApprovalTests.Core;
using ApprovalTests.Core.Exceptions;
using ApprovalTests.Namers;
using ApprovalTests.Reporters;
using ApprovalTests.Set;
using ApprovalTests.Writers;
using NUnit.Framework;

namespace ImmutableClass.Framework.Test
{
    [UseReporter(typeof(ClipboardReporter))]
    public class Class1
    {
        static string[] _classFiles = Directory.GetFiles(Path.Combine(TestContext.CurrentContext.TestDirectory, "Input"));

        [Test]
        [TestCaseSource(nameof(_classFiles))]
        public void TheTest(string filename)
        {
            CompilationApprover.Verify(filename);
            //Approvals.Verify(new CompilationWriter(filename));
        }
    }

    public class CompilationApprover : IApprovalApprover
    {
        readonly CompilationWriter _writer;
        readonly IApprovalNamer _namer;
        string _approved;
        ApprovalException _failure;
        string _received;

        public static void Verify(string filename)
        {
            var compiled = "";
            
            var writer = new CompilationWriter(compiled, filename);
            var namer = Approvals.GetDefaultNamer();
            var reporter = Approvals.GetReporter();

            Approver.Verify(new CompilationApprover(writer, namer), reporter);
        }

        public CompilationApprover(CompilationWriter writer, IApprovalNamer namer)
        {
            _writer = writer;
            _namer = namer;
        }

        public bool Approve()
        {
            string basename = $@"{_namer.SourcePath}\{_namer.Name}";
            _approved = Path.GetFullPath(_writer.GetApprovalFilename(basename));
            _received = Path.GetFullPath(_writer.GetReceivedFilename(basename));
            _received = _writer.WriteReceivedFile(_received);

            _failure = Approve(_approved, _received);
            return _failure == null;
        }

        public static ApprovalException Approve(string approved, string received)
        {
            if (File.Exists(approved) == false)
            {
                return new ApprovalMissingException(received, approved);
            }

            //var process = new Process();
            ////settings up parameters for the install process
            //process.StartInfo.FileName = "diff-pdf";
            //process.StartInfo.Arguments = String.Format("\"{0}\" \"{1}\"", received, approved);

            //process.Start();

            //process.WaitForExit();

            //if (process.ExitCode != 0)
            //{
            //    return new ApprovalMismatchException(received, approved);
            //}

            return null;
        }

        public void Fail()
        {
            throw _failure;
        }

        public void ReportFailure(IApprovalFailureReporter reporter)
        {
            reporter.Report(_approved, _received);
        }

        public void CleanUpAfterSuccess(IApprovalFailureReporter reporter)
        {
            File.Delete(_received);
            if (reporter is IApprovalReporterWithCleanUp up)
            {
                up.CleanUp(_approved, _received);
            }
        }
    }
    

    public class CustomNamer : IApprovalNamer
    {
        public string SourcePath { get; }
        public string Name { get; }
    }

    public class CompilationWriter : ApprovalTextWriter
    {
        readonly string _filename;

        public CompilationWriter(string data, string filename)
            : base(data, "cs")
        {
            _filename = filename;
        }

        public override string GetApprovalFilename(string basename)
        {
            var approvalFilename = Path.Combine(Path.GetDirectoryName(basename), $"{_filename}{WriterUtils.Approved}{ExtensionWithDot}");
            if (File.Exists(approvalFilename) == false) File.Create(approvalFilename);
            return approvalFilename ;
        }

        public override string GetReceivedFilename(string basename)
        {
            return Path.Combine(Path.GetDirectoryName(basename), $"{_filename}{WriterUtils.Received}{ExtensionWithDot}");
        }
    }
}
