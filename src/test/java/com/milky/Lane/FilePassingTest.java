package com.milky.Lane;

import com.milky.Lane.exception.LaneException;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class FilePassingTest {
    public static void main(String[] args) {
        var path = "./src/test/lane";
        var fileDecls = readFiles(path);
        for (var fileDecl : fileDecls) {
            System.out.println(fileDecl.fileName());
            var ast = Lane.parseLaneProgram(fileDecl.fileContent());

            try {
                ast.typeCheck();
                var val = ast.evaluateK();
                System.out.println(val);
            } catch (LaneException e) {
                System.out.println(e.message);
            }
            System.out.println("\n");
        }
    }

    static List<TestFileDecl> readFiles(String path) {
        var fileDecls = new ArrayList<TestFileDecl>();
        var dir = new File(path);
        var files = dir.listFiles();
        if (files != null) {
            for (var f : files) {
                fileDecls.add(readFile(f.getPath()));
            }
        }
        return fileDecls;
    }

    static TestFileDecl readFile(String path) {
        var sb = new StringBuilder();
        try {
            var fileInputStream = new FileInputStream(path);
            var inputStreamReader = new InputStreamReader(fileInputStream);
            var bufferedReader = new BufferedReader(inputStreamReader);
            String line = bufferedReader.readLine();
            while (line != null) {
                sb.append(line).append("\n");
                line = bufferedReader.readLine();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return new TestFileDecl(path, sb.toString());
    }
}

record TestFileDecl(String fileName, String fileContent) {}
