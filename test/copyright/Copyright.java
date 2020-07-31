/*
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
*/

import java.io.*;
import java.lang.String.*;
import java.nio.file.*;
import java.util.*;
import java.util.regex.*;

public class Copyright {
    public static String lineBreakMatcher = "\\R";
    public static String beginningInput   = "\\A";
    public static String optionalShebang  = "(#!\\p{Graph}+" + lineBreakMatcher + ")?"; // [^\R] is not accepted: Illegal/unsupported escape sequence near index 8

    public static void main(String[] args) {
        if (args.length != 1) {
            System.err.println("Usage: copyright <path>");
            System.exit(-1);
        }

        Path path = Paths.get(args[0]);
        String[] copyright = { Pattern.quote("TorXakis - Model Based Testing")
                             , "Copyright \\(c\\) (?:\\d{4}\\-)?\\d{4} .*"
                             , Pattern.quote("See LICENSE at root directory of this repository.")
                             };

        Copyright c = new Copyright();
        c.addExtensionCopyrightHandler(".hs",      new CopyrightInCommentSection("{-", "-}", copyright));
        c.addExtensionCopyrightHandler(".x",       new CopyrightInCommentLines  ("--", copyright));
        c.addExtensionCopyrightHandler(".y",       new CopyrightInCommentSection("{-", "-}", copyright));
        c.addExtensionCopyrightHandler(".txs",     new CopyrightInCommentSection("{-", "-}", copyright));
        c.addExtensionCopyrightHandler(".java",    new CopyrightInCommentSection("/*", "*/", copyright));
        c.addExtensionCopyrightHandler("Makefile", new CopyrightInCommentLines  ("#", copyright));
        c.addExtensionCopyrightHandler(".yaml",    new CopyrightInCommentLines  ("#", copyright));
        c.addExtensionCopyrightHandler(".sh",      new CopyrightInCommentLines  (optionalShebang, "#", copyright));
        c.addExtensionCopyrightHandler(".yml",     new CopyrightInCommentLines  ("#", copyright));
        c.addExtensionCopyrightHandler(".bat",     new CopyrightInCommentLines  ("@REM", copyright));
        c.addExtensionCopyrightHandler(".txt",     new CopyrightInText          (copyright));

        try {
            c.walkFiles(path);
            System.out.println("#Files = " + c.getFiles());
            System.out.println("#Violations = " + c.getCount());
            if (c.isCorrect()) {
                System.out.println("Pass");
            } else {
                System.out.println("Fail");
                System.exit(-1);
            }
        } catch (IOException e) {
            e.printStackTrace();
            System.out.println("Fail");
        }
    }

    private void addExtensionCopyrightHandler(String extension, CopyrightHandler handler) {
        map.put(extension, handler);
    }


    private Map<String, CopyrightHandler> map = new HashMap<>();

    private int count = 0;
    private int getCount() {
        return count;
    }
    private boolean isCorrect() {
        return count == 0;
    }

    private int files = 0;
    private int getFiles() {
        return files;
    }

    private void walkFiles(Path path) throws IOException {
        try (DirectoryStream<Path> stream = Files.newDirectoryStream(path)) {
            for (Path entry : stream) {
                if (Files.isDirectory(entry)) {
                    if (entry.getNameCount() > 0) {
                        String name = entry.getName(entry.getNameCount()-1).toString();
                        if (!name.startsWith(".")) // skip cache folders
                            walkFiles(entry);
                        }
                } else {
                    assert Files.isRegularFile(entry) : "Not a directory is a file";

                    if (!entry.endsWith("license.txt")) {
                        for (String key : map.keySet()) {
                            if (entry.toString().endsWith(key)) {
                                files += 1;
                                CopyrightHandler handler = map.get(key);
                                if (!handler.checkCopyright(entry))
                                {
                                    System.out.println(entry.toString());
                                    count ++;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

interface CopyrightHandler {
    public boolean checkCopyright (Path p) throws IOException;
}

abstract class CopyrightPatternHandler implements CopyrightHandler {
    protected Pattern copyrightPattern;

    protected String joinText (final String [] txt)
    {
        return String.join (Copyright.lineBreakMatcher, txt);
    }

    protected String startWith (final String s)
    {
        return Copyright.beginningInput + s + ".*";
    }

    public boolean checkCopyright (Path p) throws IOException
    {
        String data = new String(Files.readAllBytes(p));
        Matcher m = copyrightPattern.matcher(data);
        return m.matches();
    }
}

class CopyrightInText extends CopyrightPatternHandler {
    public CopyrightInText (final String[] txt) {
        copyrightPattern = Pattern.compile(startWith(joinText(txt)), Pattern.DOTALL);
    }
}

class CopyrightInCommentSection extends CopyrightPatternHandler {
    public CopyrightInCommentSection (final String open, final String close, final String[] txtLines) {
        List<String> retList = new ArrayList<>();
        retList.add(Pattern.quote(open));
        Collections.addAll(retList, txtLines);
        retList.add(Pattern.quote(close));
        copyrightPattern = Pattern.compile(startWith(joinText(retList.toArray(new String[0]))), Pattern.DOTALL);
    }
}

class CopyrightInCommentLines extends CopyrightPatternHandler {
    private String[] toCommentLines (final String comment, final String[] txtLines) {
        List<String> retList = new ArrayList<>();
        for (String line : txtLines) {
            retList.add(Pattern.quote(comment) + " " + line);
        }
        return retList.toArray(new String[0]);
    }

    public CopyrightInCommentLines (final String comment, final String[] txtLines) {
        copyrightPattern = Pattern.compile(startWith(joinText(toCommentLines(comment, txtLines))), Pattern.DOTALL);
    }

    public CopyrightInCommentLines (final String header, final String comment, final String[] txtLines) {
        copyrightPattern = Pattern.compile(startWith(header + joinText(toCommentLines(comment, txtLines))), Pattern.DOTALL);
    }
}