#I @"../../packages/build/FAKE/tools"
#r @"FakeLib.dll"
#load @"Paths.fsx"
#load @"Helpers.fsx"

#I @"../../packages/build/Microsoft.CodeAnalysis.Common/lib/net45"
#I @"../../packages/build/Microsoft.CodeAnalysis.CSharp/lib/net45"
#r @"Microsoft.CodeAnalysis.CSharp.dll"
#r @"Microsoft.CodeAnalysis.dll"

open System
open Fake 
open Paths
open Helpers
open System.IO
open System.Threading.Tasks
open System.Threading
open Microsoft.FSharp.Reflection
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

module Documentation = 

    type Extensions = | Cs | AsciiDoc | Png | Gif

    type DocumentationFile = 
        | AsciiFile  of FileInfo 
        | RoslynFile of FileInfo 
        | RawFile of FileInfo

        static member FromPath f = 
            let fileInfo = FileInfo f
            match Unions.fromString<Extensions>(fileInfo.Extension) with
            | Some Cs ->  RoslynFile fileInfo
            | Some AsciiDoc -> AsciiFile fileInfo
            | _ -> RawFile fileInfo

        static member Scan directory = 
            let skipFolders = ["Nest.Tests.Literate"; "Debug"; "Release"]
            let inBadDir file = skipFolders |> List.contains((DirectoryInfo file).Parent.Name) |> not
            Unions.toSeq<Extensions>
            |> Seq.map(fun ext -> sprintf "*.%s" (ext.ToLowerInvariant()))
            |> Seq.map(fun pattern -> Directory.GetFiles(directory, pattern, SearchOption.AllDirectories))
            |> Seq.concat
            |> Seq.filter inBadDir
            |> Seq.map DocumentationFile.FromPath

        static member OutputExtension ext =
            match Unions.fromString<Extensions>(ext) with
            | Some Cs -> "asciidoc"
            | _ -> ext

    let docFolder = "docs/contents/new2"

    let LitUp = fun _ ->
        let files = DocumentationFile.Scan (directoryInfo "src/Tests").FullName
        let outputFile(inputFile: FileInfo) = 
            let ext = inputFile.Extension
            let re = @"(^.+\\Tests\\|\" + ext + "$)"
            let inPath = (regex_replace re "" inputFile.FullName) + (DocumentationFile.OutputExtension ext)

            let outPath = FileInfo <| Path.GetFullPath(Path.Combine(docFolder, inPath));
            if not outPath.Directory.Exists then
                outPath.Directory.Create()
            outPath           

        let export (doc: DocumentationFile) = 
            match doc with 
            | AsciiFile file | RawFile file -> 
                file.CopyTo((outputFile file).FullName) |> ignore
            | RoslynFile file ->
                file.CopyTo((outputFile file).FullName) |> ignore

        files |> Seq.iter export

    type TextBlock = {  LineNumber: int; Text: string }
    type CodeBlock = {  LineNumber: int; Code: string }
    type SingleBlock =  | TextBlock | CodeBlock
    type CombinedBlock = {  LineNumber: int; Code: seq<SingleBlock> }
    type DocumentationBlock =  | TextBlock | CodeBlock | CombinedBlock

    type DocumentationFileWalker() =
        inherit CSharpSyntaxWalker(Microsoft.CodeAnalysis.SyntaxWalkerDepth.StructuredTrivia)

        let classDepth = ref 0;
        let insideMultiLineDocumentation = ref false;
        let insideAutoInfludeMethodBlock = ref false;
        let insideDslExample = ref false;

        let mutable blocks: List<DocumentationBlock> = []
        let addBlock block = 
            let newBlocks = block :: blocks
            blocks <- newBlocks

        member this.Blocks = blocks |> List.rev

        override this.VisitClassDeclaration node =
            incr classDepth
            base.VisitClassDeclaration node
            decr classDepth
        
        override this.VisitPropertyDeclaration node =
            match node.Identifier.Text with 
            | "Fluent" | "Initializer" -> 
                insideDslExample := true
                base.VisitPropertyDeclaration node
                insideDslExample := false
            | _ -> ignore()
        
        override this.VisitMethodDeclaration node =
            if !classDepth = 1 then insideAutoInfludeMethodBlock := true
            base.VisitMethodDeclaration node
            insideAutoInfludeMethodBlock := false

        override this.VisitXmlText node =

            let text = node.TextTokens
                       |> Seq.filter (fun n -> n.Kind() = SyntaxKind.XmlTextLiteralToken)
                       |> Seq.fold(fun (sb:System.Text.StringBuilder) s -> sb.Append(s)) (new System.Text.StringBuilder())
            
            let lineSpan = node.SyntaxTree.GetLineSpan(node).StartLinePosition.Line

            base.VisitXmlText node