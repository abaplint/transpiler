// Dummy Monaco hover provider for demonstration
import * as monaco from 'monaco-editor';
import * as sourceMap from "source-map";


export class HoverProvider implements monaco.languages.HoverProvider {
  private json: sourceMap.RawSourceMap | undefined = undefined;
  private consumer: sourceMap.BasicSourceMapConsumer | undefined = undefined;
  private readonly editor1: monaco.editor.IStandaloneCodeEditor;
  private readonly editor2: monaco.editor.IStandaloneCodeEditor;

  public constructor(editor1: monaco.editor.IStandaloneCodeEditor, editor2: monaco.editor.IStandaloneCodeEditor) {
    // @ts-ignore
    sourceMap.SourceMapConsumer.initialize({
      'lib/mappings.wasm': "https://unpkg.com/source-map@0.7.3/lib/mappings.wasm",
    });

    this.editor1 = editor1;
    this.editor2 = editor2;
  }

  public setMap(json: string) {
    this.json = JSON.parse(json) as sourceMap.RawSourceMap;

    new sourceMap.SourceMapConsumer(this.json).then((consumer: sourceMap.BasicSourceMapConsumer) => {
      console.log("parsed json source map");
      this.consumer = consumer;
    });
  }

  public provideHover(model: monaco.editor.ITextModel, position: monaco.Position): monaco.languages.ProviderResult<monaco.languages.Hover> {
    console.log("hover: " + model.uri + " " + position.lineNumber + ":" + position.column);

    const decorations: monaco.editor.IModelDeltaDecoration[] = [];
    if (model.getLanguageId() === "abap") {
      console.log("source file hover");
      const pos: sourceMap.MappedPosition = {
        source: this.json?.sources[0] || "",
        line: position.lineNumber,
        column: position.column,
      };
      const found = this.consumer?.allGeneratedPositionsFor(pos);
      console.log("found: " + found?.length + " positions");

      for (const f of found || []) {
        console.log(JSON.stringify(f));
        if (f.line === null || f.column === null) {
          continue;
        }

        let className = "myGreenDecoration";
        if (f.lastColumn === null) {
          const lineContent = this.editor2.getModel().getLineContent(f.line);
          f.lastColumn = lineContent.length - 1; // Monaco is 1-based, so we need to adjust
          console.log(f.lastColumn);
          className = "myRedDecoration";
        }

        decorations.push({
          range: new monaco.Range(f.line, f.column + 1, f.line, f.lastColumn + 2),
          options: {
            inlineClassName: className,
          },
        }
        );
        /*
        decorations.push({range: new monaco.editor.Range(
          new vscode.Position(f.line-1, f.column),
          new vscode.Position(f.line-1, f.lastColumn+1))}
        );
        */
      }
      this.editor2.removeDecorations(this.editor2.getModel().getAllDecorations().map(d => d.id));
      this.editor2.createDecorationsCollection(decorations);
 /*

      const editors = vscode.window.visibleTextEditors;
      for (const editor of editors) {
        if (Utils.basename(editor.document.uri) === json?.file) {
          editor.setDecorations(green, decorations);
        } else {
          editor.setDecorations(green, []);
        }
      }
        */
    } else if (model.getLanguageId() === "javascript") {
      console.log("generated file hover");
      /*
      const pos: sourceMap.Position = {
        line: position.lineNumber + 1,
        column: position.column,
      };
      const found = consumer?.originalPositionFor(pos);
      console.log("found: " + JSON.stringify(found));
      if (found && found.line && found.column) {
        decorations.push({range: new vscode.Range(
          new vscode.Position(found.line-1, found.column),
          new vscode.Position(found.line-1, found.column+1))}
        );
      }

      const editors = vscode.window.visibleTextEditors;
      for (const editor of editors) {
        if (Utils.basename(editor.document.uri) === found?.source) {
          editor.setDecorations(red, decorations);
        } else {
          editor.setDecorations(red, []);
        }
      }
*/
    } else {
      console.log("File is not valid for opened map");
    }

    return undefined;
  }

}