"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.activate = void 0;
const vscode = require("vscode");
const axios_1 = require("axios"); // axios is a popular HTTP client
// Example URL of the web service exposing your Haskell functionality
const API_URL = 'http://localhost:8080/findHexCode';
function findHexCode(text) {
    return __awaiter(this, void 0, void 0, function* () {
        try {
            // Call the web service with the text to find its hex code
            const response = yield axios_1.default.get(`${API_URL}?text=${encodeURIComponent(text)}`);
            if (response.data && response.data.hexCode) {
                return response.data.hexCode;
            }
            return null;
        }
        catch (error) {
            console.error('Error calling findHexCode API:', error);
            return null;
        }
    });
}
function activate(context) {
    let disposable = vscode.languages.registerHoverProvider('yourLanguageId', {
        provideHover(document, position) {
            return __awaiter(this, void 0, void 0, function* () {
                const wordRange = document.getWordRangeAtPosition(position);
                const word = document.getText(wordRange);
                const hexCode = yield findHexCode(word);
                if (!hexCode)
                    return;
                // Use a simple CSS style for color preview instead of the Color library
                const markdownString = new vscode.MarkdownString(`![color preview](https://via.placeholder.com/15/${hexCode.slice(1)}/${hexCode.slice(1)}.png) ${hexCode}`);
                markdownString.isTrusted = true; // Allows for rendering images, links, etc.
                return new vscode.Hover(markdownString);
            });
        }
    });
    context.subscriptions.push(disposable);
}
exports.activate = activate;
function deactivate() { }
//# sourceMappingURL=extension.js.map