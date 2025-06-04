// Dummy Monaco hover provider for demonstration
import * as monaco from 'monaco-editor';


export function registerDummyHoverProvider(languageId: string) {
  monaco.languages.registerHoverProvider(languageId, {
    provideHover: function(model, position) {
      return {
        range: new monaco.Range(position.lineNumber, 1, position.lineNumber, model.getLineMaxColumn(position.lineNumber)),
        contents: [
          {value: '**Dummy Hover**'},
          {value: 'This is a dummy hover tooltip.'},
        ],
      };
    },
  });
}