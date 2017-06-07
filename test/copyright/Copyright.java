/*
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
*/

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Copyright {
	private final static boolean solve = false;

	public final static String copyright = 
			"TorXakis - Model Based Testing\n" +
			"Copyright (c) 2015-2016 TNO and Radboud University\n" +
			"See license.txt";

	private static String commentLineBased(final String comment, final String txt)
	{
		String [] lines = txt.split("\n");
		return comment + " " + String.join("\n" + comment + " ", lines) + "\n\n";
	}
	
	private static String commentSectionBased(final String open, final String close, final String txt)
	{

		return open + "\n" + txt + "\n" + close + "\n\n";
	}

	private Map<String, String> map = new HashMap<String,String>();
	private int count = 0;
	public final int getCount() {
		return count;
	}

	private boolean isCorrect()
	{
		return count == 0;
	}

	public final int getFiles() {
		return files;
	}
	private int files = 0;

	void addExtensionCommentedCopyright(String extension, String commentedCopyright)
	{
		map.put(extension, commentedCopyright);
	}

	void walkFiles (Path path) throws IOException
	{
		try (DirectoryStream<Path> stream = Files.newDirectoryStream(path)) 
		{
			for(Path entry : stream)
			{
				if (Files.isDirectory(entry))
				{
                    if (!entry.endsWith(".stack-work"))    // skip stack folders
                        walkFiles(entry);
				}
				else
				{
					assert Files.isRegularFile(entry) : "Not a directory is a file";

					if (entry.endsWith("license.txt"))
					{
						//skip
					}
					else
					{
						boolean found = false;
						for (String key : map.keySet())
						{
							if (entry.toString().endsWith(key))
							{
								found = true;
								files += 1;

								String commentedCopyright = map.get(key);
								String[] expected = commentedCopyright.split("\n");

								List<String> lines = Files.readAllLines(entry);
								String [] actual = lines.toArray(new String[0]);

								if (actual.length < expected.length)
								{
									handledErroneousFile(entry, commentedCopyright);
								}
								else
								{
									int i = 0;
									int j = expected.length;
									while (i != j)
									{
										//System.out.println("Expected[i]" + expected[i]);
										//System.out.println("Actual[i]" + actual[i]);
										if (expected[i].equals(actual[i]))
										{
											i = i + 1;
										}
										else
										{
											j = i;
										}
									}
									if ( i != expected.length )
									{
										handledErroneousFile(entry, commentedCopyright);
									}
								}
							}
						}
						if (!found)
						{
							//System.out.println("Skipped: "+ entry.toString());
						}
					}
				}
			}
		}
	}

	private void handledErroneousFile(Path entry, String commentedCopyright) throws IOException {
		count += 1;	
		System.out.println(entry.toString());
		if (solve)
		{
			addComment(entry, commentedCopyright);
		}
	}

	private void addComment(Path entry, String commentedCopyright) throws IOException {
		Path backup = Paths.get(entry.toString()+".orig");
		Files.move(entry, backup);
		byte[] content = Files.readAllBytes(backup);
		Files.write(entry, commentedCopyright.getBytes());
		Files.write(entry, content, StandardOpenOption.APPEND);
	}

	public static void main(String[] args) {
		if (args.length != 1)
		{
			System.err.println("Usage: copyright <path>");
			System.exit (-1);
		}

		Path path = Paths.get(args[0]);

		Copyright c = new Copyright();
		c.addExtensionCommentedCopyright(".hs", commentSectionBased("{-", "-}", copyright));
		c.addExtensionCommentedCopyright(".x", commentLineBased("--", copyright));
		c.addExtensionCommentedCopyright(".y", commentSectionBased("{-", "-}", copyright));
		c.addExtensionCommentedCopyright(".txs", commentSectionBased("{-", "-}", copyright));
		c.addExtensionCommentedCopyright(".java", commentSectionBased("/*", "*/", copyright));
		c.addExtensionCommentedCopyright("Makefile", commentLineBased("#", copyright));
		c.addExtensionCommentedCopyright(".txt", copyright + "\n\n");

		try {
			c.walkFiles(path);
			System.out.println("#Files = " + c.getFiles());
			System.out.println("#Violations = " + c.getCount());
			if (c.isCorrect())
			{
				System.out.println("Pass");
			}
			else
			{
				System.out.println("Fail");
			}
		} catch (IOException e) {
			e.printStackTrace();
			System.out.println("Fail");
		}
	}

}
