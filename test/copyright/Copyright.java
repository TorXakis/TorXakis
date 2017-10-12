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

    private static String[] copyright = new String[0];

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

        try {
            copyright = Files.readAllLines(copyrightFile.toPath()).toArray(copyright);
            System.out.println("Searching for Copyright notice:\n" + String.join("\n", copyright) + "\n");
        } catch (IOException e) {
            System.err.println("Can't read copyright file at " + copyrightFile.toString());
            e.printStackTrace();
            System.exit(-1);
        }

        Copyright c = new Copyright();
        c.addExtensionCommentedCopyright(".hs", commentSectionBased("{-", "-}", copyright));
        c.addExtensionCommentedCopyright(".x", commentLineBased("--", copyright));
        c.addExtensionCommentedCopyright(".y", commentSectionBased("{-", "-}", copyright));
        c.addExtensionCommentedCopyright(".txs", commentSectionBased("{-", "-}", copyright));
        c.addExtensionCommentedCopyright(".java", commentSectionBased("/*", "*/", copyright));
        c.addExtensionCommentedCopyright("Makefile", commentLineBased("#", copyright));
        c.addExtensionCommentedCopyright(".yaml", commentLineBased("#", copyright));
        c.addExtensionCommentedCopyright(".sh", commentLineBased("#", copyright));
        c.addExtensionCommentedCopyright(".yml", commentLineBased("#", copyright));
        c.addExtensionCommentedCopyright(".bat", commentLineBased("@REM", copyright));
        c.addExtensionCommentedCopyright(".txt", copyright);

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

    private void addExtensionCommentedCopyright(String extension, String[] commentedCopyright) {
//		System.out.println("Notice for '" + extension + "' is:\n" + String.join("\n", commentedCopyright));
        map.put(extension, commentedCopyright);
    }

    private static String[] commentLineBased(final String comment, final String[] txtLines) {
        List<String> retList = new ArrayList<>();
        for (String line : txtLines) {
            retList.add(comment + " " + line);
        }
        return retList.toArray(new String[0]);
    }

    private static String[] commentSectionBased(final String open, final String close, final String[] txt) {
        List<String> retList = new ArrayList<>();
        retList.add(open);
        Collections.addAll(retList, txt);
        retList.add(close);
        return retList.toArray(new String[0]);
    }

    private Map<String, String[]> map = new HashMap<>();
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
                                files += 1;

                                String[] commentedCopyright = map.get(key);

                                List<String> lines = Files.readAllLines(entry);
                                String[] actual = lines.toArray(new String[0]);

                                if (actual.length < commentedCopyright.length) {
                                    handledErroneousFile(entry, commentedCopyright);
                                } else {
                                    int i = 0;
                                    int j = commentedCopyright.length;
                                    while (i != j) {
//										System.out.println("Expected[i]" + commentedCopyright[i]);
//										System.out.println("Actual[i]" + actual[i]);
                                        if (commentedCopyright[i].equals(actual[i])) {
                                            i = i + 1;
                                        } else {
                                            j = i;
                                        }
                                    }
                                    if (i != commentedCopyright.length) {
                                        handledErroneousFile(entry, commentedCopyright);
                                    }
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
                    && (allLines[0].contains(copyright[0])
                    || allLines[1].contains(copyright[0]))) {
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
