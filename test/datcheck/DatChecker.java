/*
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
*/


import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class DatChecker {
    private static List<String> fileContents = new ArrayList<String>();
    
    public static void main(String[] args) {
        if ( args.length != 1 ) {
            System.out.println( "Usage: DatChecker <dat output file>" );
            return;
        }
        
        readFile( args[0] );
        
        System.out.println("Checking development acceptance test output for file " + args[0] );
        
        doCheck();
        
        System.out.println("Checking finished");
    }
    
    private static void readFile( String fileName ) {
        try {
            String line;
            BufferedReader bufferedReader;

            bufferedReader = new BufferedReader( new FileReader( fileName ) );

            while ((line = bufferedReader.readLine()) != null)
            {
                fileContents.add(line);
            }
           
            // close the BufferedReader when we're done
            bufferedReader.close();
        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    private static void doCheck() {
        System.out.println( "- TestCopyright:         " + checkTestCopyright() );
        System.out.println( "- Test build:            " + checkTestBuild() );
        System.out.println( "- Torxakis build:        " + checkTorxakisBuild() );
        System.out.println( "- SelfTests:             " + checkSelfTest() );
        System.out.println( "- TestExamps:            "); checkTestExamps();
    }

    private static void checkTestExamps() {
        System.out.println( "--- Stimulus Response Tests");
        checkStimulusResponseTests();
        
        System.out.println( "--- CustomerOrders Tests");
        checkCustomersOrdersTests();
        
        System.out.println( "--- LuckyPeople Tests:");
        checkLuckyPeopleTests();
    }

    private static void checkStimulusResponseTests() {
        System.out.println( "----- StimulusResponse Test 1: " + checkTorXakisTest( "StimulusResponse Test 1", "PASS" ) );
        System.out.println( "----- StimulusResponse Test 2: " + checkTorXakisTest( "StimulusResponse Test 2", "FAIL" ) );
        System.out.println( "----- StimulusResponse Test 3: " + checkTorXakisTest( "StimulusResponse Test 3", "PASS" ) );
        System.out.println( "----- StimulusResponse Test 4: " + checkTorXakisTest( "StimulusResponse Test 4", "FAIL" ) );
        System.out.println( "----- StimulusResponse Test 5: " + checkTorXakisTest( "StimulusResponse Test 5", "PASS" ) );
    }

    private static void checkCustomersOrdersTests() {
        System.out.println( "----- CustomersOrders Test: " + checkTorXakisTest( "CustomersOrders Test", "PASS" ) );
        
    }

    private static void checkLuckyPeopleTests() {
        System.out.println( "----- LuckyPeopleTest Test 1: " + checkTorXakisTest( "LuckyPeople Test 1", "PASS" ) );
        System.out.println( "----- LuckyPeopleTest Test 2: " + checkTorXakisTest( "LuckyPeople Test 2", "PASS" ) );
        
    }

    private static String checkTorXakisTest(String testName, String expectedResult) {
        String[] searchStrings = {
                "^------- Start " + testName +"$",
                "^TXS >>  TorXakis :: Model-Based Testing$",
                "^TXS >>  " + expectedResult,
                "^TXS >>  TorXakis :: Model-Based Testing  << End$",
                 "^------- End " + testName + "$"
        };
        
        return findInOrder( searchStrings );
    }

    private static String checkTestBuild()
    {
        return findPatternOnly ( "^--- Start Test Java$"
                               , "^javac -sourcepath "
                               , "^--- End Test Java$"
                               );
    }
    
    private static String checkTorxakisBuild() {
        String[] searchStrings = {
            "^--- Start TorXakis Build$",
            "txsserver.exe$", 
            "txsui.exe$",
            "^--- End TorXakis Build$"
        };
        return findInOrder( searchStrings );
    }

    private static String checkSelfTest( ) {
        String[] searchStrings = { 
                "^Completed (\\d+) action\\(s\\)\\.$",
                "^Log files have been written to: ",
                "^--- End Self Test$"
            };
            
        return findConsecutive( searchStrings ); 
    }

    private static String checkTestCopyright() {
        String[] searchStrings = { 
            "^#Violations = 0$", 
            "^Pass$" 
        };
        
        return findConsecutive( searchStrings ); 
    }

    private static String findPatternOnly(String start, String match, String end) {
        Iterator<String> listIterator = fileContents.iterator();
        
        Pattern patternStart = Pattern.compile( start );
        while ( !patternStart.matcher( listIterator.next() ).find() 
                && listIterator.hasNext() ) ;
        
        if (listIterator.hasNext()) {
            Pattern patternMatch = Pattern.compile( match );
            String item = listIterator.next();
            while ( patternMatch.matcher( item ).find() 
                    && listIterator.hasNext() )
            {
                item = listIterator.next();
            }
            
            Pattern patternEnd = Pattern.compile( end );
            if ( patternEnd.matcher( item ).find()  )
                return "PASS";
        }
        
        return "FAIL";
    }
    
    private static String findConsecutive(String[] searchStrings) {
        int searchStringIndex = 0;
        
        for ( String line: fileContents ) {
            Pattern pattern = Pattern.compile( searchStrings[ searchStringIndex ] );
            Matcher matcher = pattern.matcher( line );

            if ( matcher.find() ) {
                searchStringIndex++;
                
                if ( searchStringIndex == searchStrings.length ) {
                    return "PASS";
                }
            }
            else {
                searchStringIndex = 0;
            }
        }
        
        return "FAIL";
    }
    
    private static String findInOrder(String[] searchStrings) {
        int searchStringIndex = 0;
        
        for ( String line: fileContents ) {
            Pattern pattern = Pattern.compile( searchStrings[ searchStringIndex ] );
            Matcher matcher = pattern.matcher( line );

            if ( matcher.find() ) {
                if ( matcher.groupCount() > 0) {
                    if ( matcher.group( 1 ).equals(matcher.group( 2 ) ) ) {
                        searchStringIndex++;

                        if ( searchStringIndex == searchStrings.length ) {
                            return "PASS";
                        }
                    }
                }
                else {
                    searchStringIndex++;

                    if ( searchStringIndex == searchStrings.length ) {
                        return "PASS";
                    }
                }


            }
        }
        
        return "FAIL";
    }

}
