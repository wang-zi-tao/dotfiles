import test from 'node:test';
import { equal, throws } from 'node:assert/strict';
import { fileUriToPath, fromLspPosition, isFileUri, pathToFileUri, toLspPosition, } from '../src/protocol.js';
test('pathToFileUri converts a Windows path', () => {
    equal(pathToFileUri('D:\\branch-master\\wpsmain\\src\\a.cpp'), 'file:///D:/branch-master/wpsmain/src/a.cpp');
});
test('pathToFileUri converts a POSIX path', () => {
    equal(pathToFileUri('/home/user/proj/src/a.rs'), 'file:///home/user/proj/src/a.rs');
});
test('fileUriToPath round-trips a Windows URI', () => {
    equal(fileUriToPath('file:///D:/branch-master/wpsmain/src/a.cpp'), 'D:\\branch-master\\wpsmain\\src\\a.cpp');
});
test('fileUriToPath round-trips a POSIX URI', () => {
    equal(fileUriToPath('file:///home/user/a.rs'), '/home/user/a.rs');
});
test('fileUriToPath encodes spaces and decodes them back', () => {
    const uri = pathToFileUri('C:\\my dir\\file.cpp');
    equal(uri, 'file:///C:/my%20dir/file.cpp');
    equal(fileUriToPath(uri), 'C:\\my dir\\file.cpp');
});
test('isFileUri detects file URIs only', () => {
    equal(isFileUri('file:///D:/a.cpp'), true);
    equal(isFileUri('untitled:foo'), false);
    equal(isFileUri('/not/a/uri'), false);
});
test('toLspPosition converts 1-based to 0-based', () => {
    deepEqual(toLspPosition(1, 1), { line: 0, character: 0 });
    deepEqual(toLspPosition(10, 5), { line: 9, character: 4 });
});
test('fromLspPosition converts 0-based to 1-based', () => {
    deepEqual(fromLspPosition({ line: 0, character: 0 }), { line: 1, character: 1 });
    deepEqual(fromLspPosition({ line: 9, character: 4 }), { line: 10, character: 5 });
});
test('fileUriToPath rejects non-file URIs', () => {
    throws(() => fileUriToPath('untitled:foo'), /not a file URI/);
});
function deepEqual(actual, expected, message) {
    const a = JSON.stringify(actual);
    const e = JSON.stringify(expected);
    if (a !== e)
        throw new Error(message ?? `expected ${e}, got ${a}`);
}
