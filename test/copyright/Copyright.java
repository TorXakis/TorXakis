/*
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
*/

import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Copyright {
    private static boolean solve = false;

    public static void main(String[] args) {
        if (args.length == 0) {
            System.err.println("Usage: copyright <path>");
            System.exit(-1);
        }

        Path path = Paths.get(args[0]);
        ArrayList<Path> ignorePaths = new ArrayList<>();
        if (args.length > 1) {
            if (args[1].endsWith("1")) {
                System.out.println("Setting 'solve' flag");
                solve = true;
            }
            if (args.length > 2) {
                String[] ignorePathStrings = args[2].split(",");
                for (String ignorePathString : ignorePathStrings) {
                    Path ignorePath = Paths.get(ignorePathString);
                    System.out.println("Ignoring path: " + ignorePath.toString());
                    ignorePaths.add(ignorePath);
                }
            }
        }

        File copyrightFile = new File(Paths.get(args[0], "copyright.txt").toString());
        if (!copyrightFile.exists()) {
            System.err.println("Can't find copyright file at " + copyrightFile.toString());
            System.exit(-1);
        }
        String[] copyright = new String[0];
        try {
            copyright = Files.readAllLines(copyrightFile.toPath()).toArray(copyright);
            System.out.println("Searching for Copyright notice:\n" + String.join("\n", copyright) + "\n");
        } catch (IOException e) {
            System.err.println("Can't read copyright file at " + copyrightFile.toString());
            e.printStackTrace();
            System.exit(-1);
        }

        Copyright c = new Copyright();
        c.addExtensionCopyrightHandler(".hs",      new CopyrightInCommentSection("{-", "-}", copyright));
        c.addExtensionCopyrightHandler(".x",       new CopyrightInCommentLines  ("--", copyright));
        c.addExtensionCopyrightHandler(".y",       new CopyrightInCommentSection("{-", "-}", copyright));
        c.addExtensionCopyrightHandler(".txs",     new CopyrightInCommentSection("{-", "-}", copyright));
        c.addExtensionCopyrightHandler(".java",    new CopyrightInCommentSection("/*", "*/", copyright));
        c.addExtensionCopyrightHandler("Makefile", new CopyrightInCommentLines  ("#", copyright));
        c.addExtensionCopyrightHandler(".yaml",    new CopyrightInCommentLines  ("#", copyright));
        c.addExtensionCopyrightHandler(".sh",      new CopyrightInCommentLines  (s -> s.startsWith("#!"), "#", copyright));
        c.addExtensionCopyrightHandler(".yml",     new CopyrightInCommentLines  ("#", copyright));
        c.addExtensionCopyrightHandler(".bat",     new CopyrightInCommentLines  ("@REM", copyright));
        c.addExtensionCopyrightHandler(".txt",     new CopyrightInText          (copyright));

        try {
            c.walkFiles(path, ignorePaths);
            System.out.println("#Files = " + c.getFiles());
            System.out.println("#Violations = " + c.getCount());
            if (c.isCorrect()) {
                System.out.println("Pass");
            } else if (solve) {
                System.out.println("Fixed");
            }else {
                System.out.println("Fail");
                System.out.println("Use /test/copyright/FixCopyrightNotices.bat to add missing copyright notices.");
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

    private int getFiles() {
        return files;
    }

    private int files = 0;

    private void walkFiles(Path path, ArrayList<Path> ignore) throws IOException {
        try (DirectoryStream<Path> stream = Files.newDirectoryStream(path)) {
            for (Path entry : stream) {
                if (Files.isDirectory(entry)) {
                    if (!entry.endsWith(".stack-work") && !ignore.contains(entry))    // skip stack folders
                        walkFiles(entry, ignore);
                } else {
                    assert Files.isRegularFile(entry) : "Not a directory is a file";

                    if (!entry.endsWith("license.txt")) {
                        for (String key : map.keySet()) {
                            if (entry.toString().endsWith(key)) {
//                              System.out.println(entry.toString() + " analyzing...");
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

    private void handledErroneousFile(Path entry, String[] commentedCopyright) {
        count += 1;
        if (solve) {
            fixComment(entry, commentedCopyright);
            System.out.println(entry.toString() + " fixed!");
        } else {
            System.out.println(entry.toString());
        }
    }

    private void fixComment(Path entry, String[] commentedCopyright) {
        try {
            List<String> allLinesList = Files.readAllLines(entry);
            String[] allLines = allLinesList.toArray(new String[0]);
            if (allLines.length >= commentedCopyright.length
//                    && (allLines[0].contains(copyright[0])
//                    || allLines[1].contains(copyright[0]))
                ) {
                updateNotice(entry, commentedCopyright, allLines);
            } else {
                addNotice(entry, commentedCopyright);
            }
        } catch (IOException e) {
            System.err.println("Error while fixing file: " + entry.toString());
            e.printStackTrace();
        }
    }

    private void addNotice(Path entry, String[] commentedCopyright) throws IOException {
        byte[] content = Files.readAllBytes(entry);
        String noticeContent = String.join("\n", commentedCopyright) + "\n\n";
        Files.delete(entry);
        Files.createFile(entry);
        Files.write(entry, noticeContent.getBytes(), StandardOpenOption.APPEND);
        Files.write(entry, content, StandardOpenOption.APPEND);
    }

    private void updateNotice(Path entry, String[] commentedCopyright, String[] allLines) throws IOException {
        System.arraycopy(commentedCopyright, 0, allLines, 0, commentedCopyright.length);
        String fileContent = String.join("\n", allLines);
        Files.delete(entry);
        Files.createFile(entry);
        Files.write(entry, fileContent.getBytes(), StandardOpenOption.APPEND);
    }
}

interface CopyrightHandler {
    public boolean checkCopyright (Path p) throws IOException;
    //public boolean deleteCopyright (Path p) throws IOException;
    //public boolean updateCopyright (Path p, String[] newCopyright) throws IOException;
}

interface SkipLine {
    public boolean skipLine (String line);
}

abstract class CopyrightByLines implements CopyrightHandler {
    protected String[] copyrightLines;
    protected SkipLine skipLine = l -> { return false; };

    public boolean checkCopyright (Path p) throws IOException
    {
        List<String> lines = Files.readAllLines(p);

        String[] actual = lines.toArray(new String[0]);

        int l = 0;
        while ( (l < actual.length) && skipLine.skipLine(actual[l]) )
        {
            l++;
        }
//      System.out.println("Skipped " + l + " lines");
        if ( (actual.length - l) < copyrightLines.length) {
            return false;
        } else {
            int i = 0;
            int j = copyrightLines.length;
            while (i != j) {
//              System.out.println("Expected["+ i + "] " + copyrightLines[i]);
//              System.out.println("Actual  ["+ i + "] " + actual[l+i]);
                if (copyrightLines[i].equals(actual[l+i])) {
                    i = i + 1;
                } else {
                    j = i;
                }
            }
            return (i == copyrightLines.length);
        }
    }
}

class CopyrightInText extends CopyrightByLines {
    public CopyrightInText (final String[] txt) {
        copyrightLines = txt;
    }
}

class CopyrightInCommentSection extends CopyrightByLines {
    public CopyrightInCommentSection (final String open, final String close, final String[] txtLines) {
        List<String> retList = new ArrayList<>();
        retList.add(open);
        Collections.addAll(retList, txtLines);
        retList.add(close);
        copyrightLines = retList.toArray(new String[0]);
    }
}

class CopyrightInCommentLines extends CopyrightByLines {
    public CopyrightInCommentLines (final String comment, final String[] txtLines) {
        List<String> retList = new ArrayList<>();
        for (String line : txtLines) {
            retList.add(comment + " " + line);
        }
        copyrightLines = retList.toArray(new String[0]);
    }
    public CopyrightInCommentLines (final SkipLine skipLine, final String comment, final String[] txtLines) {
        this.skipLine = skipLine;
        List<String> retList = new ArrayList<>();
        for (String line : txtLines) {
            retList.add(comment + " " + line);
        }
        copyrightLines = retList.toArray(new String[0]);
    }
}