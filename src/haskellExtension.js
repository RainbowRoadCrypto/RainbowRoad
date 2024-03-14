const vscode = require('vscode');
const { exec } = require('child_process');

/**
 * Invokes a Haskell program to visualize the input word.
 * @param {string} word - The word to visualize.
 * @param {Function} callback - Callback function to handle the visualization result.
 */
function invokeHaskellVisualizeInput(word, callback) {
    // Properly escape the input word to prevent command injection
    const sanitizedWord = word.replace(/"/g, '\\"');
    const command = `visualizeInputHaskell "${sanitizedWord}"`;

    exec(command, (error, stdout, stderr) => {
        if (error) {
            console.error('Error invoking Haskell visualization:', error);
            return callback(`Error visualizing input: ${error.message}`);
        }
        callback(stdout.trim()); // Trim to remove any trailing newline
    });
}

/**
 * Activates the extension.
 * @param {vscode.ExtensionContext} context - The extension context.
 */
function activate(context) {
    let hoverProvider = vscode.languages.registerHoverProvider('yourLanguageId', {
        provideHover(document, position, token) {
            const range = document.getWordRangeAtPosition(position);
            const word = document.getText(range);

            return new Promise((resolve, reject) => {
                invokeHaskellVisualizeInput(word, (visualizationInfo) => {
                    // Use MarkdownString for better formatting options
                    const markdown = new vscode.MarkdownString(visualizationInfo);
                    markdown.isTrusted = true; // Allows for executing commands within the Markdown
                    resolve(new vscode.Hover(markdown));
                });
            });
        }
    });

    context.subscriptions.push(hoverProvider);
}

function deactivate() {}

module.exports = {
    activate,
    deactivate
};
