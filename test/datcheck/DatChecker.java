/*
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
*/

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class DatChecker {
    private static final String DEBUG_FILE_NAME = "DatChecker.debug.log";
    private static boolean DEBUG = false;
    private static PrintWriter DebugWriter;

    public static void main(String[] args) {
        if (args.length == 0) {
            System.out.println("Usage: DatChecker <dat output file> [-debug]");
            return;
        }

        if (args.length == 2 && args[1].equals("-debug")) {
            DEBUG = true;
            try {
                DebugWriter = new PrintWriter(DEBUG_FILE_NAME);
                DebugWriter.println("Processing: " + args[0]);
            } catch (IOException e) {
                System.err.println("WARNING: Can't open debug output file: " + DEBUG_FILE_NAME);
                e.printStackTrace();
            }
        }

        List<String> fileContents;
        try {
            fileContents = readFile(args[0]);
        } catch (IOException e) {
            System.err.println("ERROR: Can't read file: " + args[0]);
            e.printStackTrace();
            return;
        }
        System.out.println("Checking development acceptance test output for file " + args[0]);
        doCheck(fileContents);
        System.out.println("Checking finished");
        if (DebugWriter != null) {
            DebugWriter.close();
        }
    }

    private static List<String> readFile(String fileName) throws IOException {
        List<String> fileContents = new ArrayList<>();
        BufferedReader bufferedReader = null;
        try {
            String line;
            bufferedReader = new BufferedReader(new FileReader(fileName));

            while ((line = bufferedReader.readLine()) != null) {
                fileContents.add(line);
            }
        } finally {
            if (null != bufferedReader) {
                bufferedReader.close();
            }
        }
        return fileContents;
    }

    private static void doCheck(List<String> fileContents) {
        System.out.println("- TestCopyright:         " + checkTestCopyright(fileContents));
        System.out.println("- Test build:            " + checkTestBuild(fileContents));
        System.out.println("- Torxakis build:        " + checkTorxakisBuild(fileContents));
        System.out.println("- SelfTests:             " + checkSelfTest(fileContents));
        System.out.println("- TestExamps:            ");
//        checkTestExamps(fileContents);


        int testExampsStartLine = fileContents.indexOf("--- Start TestExamps");
        debug("Test Examps start at: " + testExampsStartLine);

        String prefixExampTestsStart = "----- Start ";
        String prefixExampTestsEnd = "----- End ";
        String prefixTestStart = "------- Start ";
        String prefixTestEnd = "------- End ";
        for (int i = testExampsStartLine + 1; i < fileContents.size(); i++) {
            String line = fileContents.get(i);
            debug("Read line " + i + ": " + line);

            if (line.startsWith(prefixExampTestsStart)) {
                String exampName = line.substring(prefixExampTestsStart.length());
                System.out.println("--- " + exampName);
                debug("Starting example: " + exampName);

                for (i++; i < fileContents.size(); i++) {
                    line = fileContents.get(i);
                    debug("Read line " + i + ": " + line);
                    if (line.startsWith(prefixTestStart)) {
                        String testName = line.substring(prefixTestStart.length());
                        debug("Starting test: " + testName);

                        Boolean passed = null;
                        for (i++; i < fileContents.size(); i++) {
                            line = fileContents.get(i);
                            debug("Read line " + i + ": " + line);
                            if (line.equals("TXS >>  PASS")) {
                                debug("Test " + testName + " has PASSED!");
                                passed = true;
                                break;
                            } else if (line.startsWith("TXS >>  FAIL")) {
                                debug("Test " + testName + " has FAILED!");
                                passed = false;
                                break;
                            } else if (line.startsWith(prefixTestEnd) ||
                                    line.startsWith(prefixTestStart) ||
                                    line.startsWith(prefixExampTestsEnd)) {
                                debug("Unexpected line; moving on for next TEST. (this test will be marked FAILED)");
                                break;
                            }
                        }
                        String result;
                        Boolean expectedTestResult = getExpectedTestResult(testName);
                        debug("Expected result for " + testName + ": " + expectedTestResult);
                        debug("Actual result for " + testName + ": " + (passed != null ? passed : "NULL"));
                        if (passed == null || passed != expectedTestResult) {
                            result = "FAIL";
                        } else {
                            result = "PASS";
                        }
                        System.out.println("----- " + testName + ": " + result);
                    } else if (line.startsWith(prefixExampTestsEnd)) {
                        debug("Moving on for next EXAMPLE.");
                        break;
                    }
                }
            }
        }
        debug("EOF.");
    }

    private static Boolean getExpectedTestResult(String testName) {
        return !(testName.equals("StimulusResponse Test 2")
                || testName.equals("StimulusResponse Test 4")
                || testName.equals("DisPro12-unique-id_Wrong Test"));
    }

    private static String checkTestBuild(List<String> fileContents) {
        return findInOrder(fileContents,
                new String[]{
                        "^--- Start Test Java$",
                        "^javac -sourcepath ",
                        "^--- End Test Java$"
                }
        );
    }

    private static String checkTorxakisBuild(List<String> fileContents) {
        String[] searchStrings = {
                "^--- Start TorXakis Install$",
                "txsserver.exe$",
                "txsui.exe$",
                "^--- End TorXakis Install$"
        };
        return findInOrder(fileContents, searchStrings);
    }

    private static String checkSelfTest(List<String> fileContents) {
        String[] searchStrings = {
                "^Completed (\\d+) action\\(s\\)\\.$",
                "^Log files have been written to: ",
                "^--- End Self Test$"
        };

        return findConsecutive(fileContents, searchStrings);
    }

    private static String checkTestCopyright(List<String> fileContents) {
        String[] searchStrings = {
                "^#Violations = 0$",
                "^Pass$"
        };

        return findConsecutive(fileContents, searchStrings);
    }

    private static String findPatternOnly(List<String> fileContents, String start, String match, String end) {
        Iterator<String> listIterator = fileContents.iterator();

        Pattern patternStart = Pattern.compile(start);
        while (!patternStart.matcher(listIterator.next()).find()
                && listIterator.hasNext()) ;

        if (listIterator.hasNext()) {
            Pattern patternMatch = Pattern.compile(match);
            String item = listIterator.next();
            while (patternMatch.matcher(item).find()
                    && listIterator.hasNext()) {
                item = listIterator.next();
            }

            Pattern patternEnd = Pattern.compile(end);
            if (patternEnd.matcher(item).find())
                return "PASS";
        }

        return "FAIL";
    }

    private static String findConsecutive(List<String> fileContents, String[] searchStrings) {
        int searchStringIndex = 0;

        for (String line : fileContents) {
            Pattern pattern = Pattern.compile(searchStrings[searchStringIndex]);
            Matcher matcher = pattern.matcher(line);

            if (matcher.find()) {
                searchStringIndex++;

                if (searchStringIndex == searchStrings.length) {
                    return "PASS";
                }
            } else {
                searchStringIndex = 0;
            }
        }

        return "FAIL";
    }

    private static String findInOrder(List<String> fileContents, String[] searchStrings) {
        int searchStringIndex = 0;

        for (String line : fileContents) {
            Pattern pattern = Pattern.compile(searchStrings[searchStringIndex]);
            Matcher matcher = pattern.matcher(line);

            if (matcher.find()) {
                if (matcher.groupCount() > 0) {
                    if (matcher.group(1).equals(matcher.group(2))) {
                        searchStringIndex++;
                        if (searchStringIndex == searchStrings.length) {
                            return "PASS";
                        }
                    }
                } else {
                    searchStringIndex++;
                    if (searchStringIndex == searchStrings.length) {
                        return "PASS";
                    }
                }
            }
        }
        return "FAIL";
    }

    private static void debug(String line) {
        if (DEBUG && DebugWriter != null) {
            DebugWriter.println(line);
        }
    }
}
