import * as vscode from 'vscode';
import axios from 'axios'; // axios is a popular HTTP client

// Example URL of the web service exposing your Haskell functionality
const API_URL = 'http://localhost:8080/findHexCode';

async function findHexCode(text: string): Promise<string | null> {
    try {
        // Call the web service with the text to find its hex code
        const response = await axios.get(`${API_URL}?text=${encodeURIComponent(text)}`);
        if (response.data && response.data.hexCode) {
            return response.data.hexCode;
        }
        return null;
    } catch (error) {
        console.error('Error calling findHexCode API:', error);
        return null;
    }
}

export function activate(context: vscode.ExtensionContext) {
    let disposable = vscode.languages.registerHoverProvider('yourLanguageId', {
        async provideHover(document: vscode.TextDocument, position: vscode.Position) {
            const wordRange = document.getWordRangeAtPosition(position);
            const word = document.getText(wordRange);
            const hexCode = await findHexCode(word);
            if (!hexCode) return;

            // Use a simple CSS style for color preview instead of the Color library
            const markdownString = new vscode.MarkdownString(`![color preview](https://via.placeholder.com/15/${hexCode.slice(1)}/${hexCode.slice(1)}.png) ${hexCode}`);
            markdownString.isTrusted = true; // Allows for rendering images, links, etc.

            return new vscode.Hover(markdownString);
        }
    });

    context.subscriptions.push(disposable);
}

function deactivate() { }

