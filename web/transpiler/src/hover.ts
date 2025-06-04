// Dummy Monaco hover provider for demonstration
import * as monaco from 'monaco-editor';


export class HoverProvider implements monaco.languages.HoverProvider {
  public provideHover(model: monaco.editor.ITextModel, position: monaco.Position): monaco.languages.ProviderResult<monaco.languages.Hover> {
    return {
      range: new monaco.Range(position.lineNumber, 1, position.lineNumber, model.getLineMaxColumn(position.lineNumber)),
      contents: [
        {value: '**Dummy Hover**'},
        {value: 'This is a dummy hover tooltip.'},
      ],
    };
  }
}