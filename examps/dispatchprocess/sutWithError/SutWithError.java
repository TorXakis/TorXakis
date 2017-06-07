/*
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
*/

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.lang.*;
import java.math.BigInteger;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Random;

import javax.swing.*;

enum State {
	Idle, Processing, Done
}

class ProcessorsStateModel {
	private static String gcd(String a, String b) {
		BigInteger b1 = new BigInteger(a);
		BigInteger b2 = new BigInteger(b);
		BigInteger gcd = b1.gcd(b2);
		return gcd.toString();
	}

	private int nrofProcessors; 
	/**
	 * @return the nrofProcessors
	 */
	public int getNrofProcessors() {
		return nrofProcessors;
	}

	private State states[];
	/**
	 * @param i		index of processor: 0 <= i < getNrofProcessors()
	 * @return the	state of processor i
	 */
	public State getState(int i) {
		return states[i];
	}
	
	private String data[][];
	public String getJobId(int i){
		return data[i][0];
	}
	public String getJobDescription(int i){
		return data[i][1];
	}
	public String getX(int i){
		return data[i][2];
	}
	public String getY(int i){
		return data[i][3];
	}
	public String getGCD(int i){
		return data[i][4];
	}

	/** Set State
	 * @param i		index of processor: 0 <= i < getNrofProcessors()
	 * @param state	the new state of the processor
	 */
	public void setState(int i, State state) {
		states[i] = state;
	}

	/** Constructor
	 * @param nrofProcessors	0 < nrOfProcessors
	 */
	public ProcessorsStateModel(int nrofProcessors) {
		this.nrofProcessors = nrofProcessors;
		states = new State[nrofProcessors];
		for (int i = 0; i < nrofProcessors; i++)
		{
			states[i] = State.Idle;
			assert State.Idle.equals(states[i]) : "initial state is Idle";
		}
		data = new String[nrofProcessors][5];
	}

	public void setData(int id, String line) {
		String[] values = line.split(",");
		assert values.length == 4 : "Wrong input";
		data[id][0] = values[0];
		data[id][1] = values[1];
		data[id][2] = values[2];
		data[id][3] = values[3];
		data[id][4] = id == 1 ? "1" : gcd(values[2], values[3]);      // bug on processor with id == 1
	}
}

class ProcessorsStateView extends JPanel implements ActionListener {

	private static final long serialVersionUID = 8547665121133548210L;
	private final int delay = 100;
	private final int width = 200; 
	private final int border = 20; 

	private final ProcessorsStateModel psm;
	private Timer timer;


	public ProcessorsStateView(ProcessorsStateModel psm) {
		this.psm = psm;

		timer = new Timer(delay,this);
		timer.start();

		setPreferredSize(new Dimension (border + psm.getNrofProcessors()*(border+width),
										2*border + width) );
	}

	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		for (int i = 0 ; i < psm.getNrofProcessors(); i++)
		{
			g.setColor(psm.getState(i).equals(State.Idle)?Color.GREEN:Color.RED);
			g.fillRect(border+(border+width)*i, border, width, width);
			if (psm.getState(i).equals(State.Processing))
			{
				g.setColor(Color.WHITE);
				g.setFont(getFont().deriveFont(44.0f));
				g.drawString(	psm.getJobId(i), 
								border + (border+width)*i + (width - g.getFontMetrics().stringWidth(psm.getJobId(i)))/2, 
								border + (width + g.getFontMetrics().getHeight())/2);
			}
		}
	}

	@Override
	public void actionPerformed(ActionEvent arg0) {
		repaint();
	}
}

class Dispatcher implements Runnable {
	private Socket socket;
	private ProcessorsStateModel model;
	public Dispatcher (Socket socket, ProcessorsStateModel model)
	{
		this.socket = socket;
		this.model = model;
	}

	@Override
	public void run() {
		try{
			// get an input stream for reading from the data socket
			InputStream inputStream= socket.getInputStream();

			// create a BufferedReader object for text line input
			BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(inputStream));

			int id = 0;
			while(true)
			{
				// read a line from the data stream: the stimulus
				String line = bufferedReader.readLine();
				if (line == null)
				{
					System.exit(-101);
				}
				System.out.println("input: "+line);

				boolean dispatched = false;
				while (! dispatched )
				{
					if (State.Idle.equals(model.getState(id)))
					{
						StringBuilder content = new StringBuilder(line);
						model.setData(id, content.substring (8, line.length() -1));
						model.setState(id, State.Processing);
						dispatched = true;
					}

					id += 1;
					if (id == model.getNrofProcessors())
						id = 0;

					try {
						Thread.sleep(1);
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
				}
			}
		}
		catch (IOException e)
		{
			System.err.println("Dispatcher");
			e.printStackTrace();
			System.exit(-3);
		}
	}
}

class Notifier implements Runnable {
	private Socket socket;
	private ProcessorsStateModel model;
	public Notifier (Socket socket, ProcessorsStateModel model)
	{
		this.socket = socket;
		this.model = model;
	}

	@Override
	public void run() {
		try{
			// get an output stream for writing to the data socket
			OutputStream outStream = socket.getOutputStream();

			// create a PrinterWriter object for character-mode output
			PrintWriter sockout = new PrintWriter(new OutputStreamWriter(outStream));

			int id = 0;
			while(true)
			{
				if (State.Done.equals(model.getState(id)))
				{
					String output = "JobOut("+ model.getJobId(id)+ "," + Integer.toString(id+1) + "," + model.getGCD(id) +")";		// Processors expected range [1..N] 
					System.out.println("output " + output);
					sockout.print(output + "\n");
					sockout.flush();
					model.setState(id, State.Idle);
				}

				id += 1;
				if (id == model.getNrofProcessors())
					id = 0;

				try {
					Thread.sleep(1);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
		}
		catch (IOException e)
		{
			System.err.println("Notifier");
			e.printStackTrace();
			System.exit(-2);
		}
	}
}

class Process implements Runnable {
    public final int fixSleepTime = 2000;
    public final int randSleepTime = 2000;
    
	private int id;
	private ProcessorsStateModel model;
	public Process (int id, ProcessorsStateModel model)
	{
		this.id = id;
		this.model = model;
	}

	@Override
	public void run() {
		Random random = new Random();
		while(true)
		{
			while (!State.Processing.equals(model.getState(id)))
			{
				try {
					Thread.sleep(100);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}

			try {
				Thread.sleep(fixSleepTime+random.nextInt(randSleepTime));
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			model.setState(id, State.Done);
		}
	}
}

public class SutWithError {

	public static final int nrofProcessors = 4;

	private static void createAndShowGUI(ProcessorsStateModel model) {
		//Create and set up the window.
		JFrame frame = new JFrame("Process");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		ProcessorsStateView view = new ProcessorsStateView(model);
		frame.add(view);

		//Display the window.
		frame.pack();
		frame.setVisible(true);
	}

	public static void main(String[] args) {
		ProcessorsStateModel model = new ProcessorsStateModel(nrofProcessors);

		try{
			//Schedule a job for the event-dispatching thread:
			//creating and showing this application's GUI.
			javax.swing.SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					createAndShowGUI(model);
				}
			});

			// instantiate a socket for accepting a connection
			ServerSocket serverSocket = new ServerSocket(7890);

			// wait to accept a connection request and a data socket is returned
			Socket socket = serverSocket.accept();

			for(int i = 0; i < model.getNrofProcessors(); i++)
			{
				Thread process = new Thread(new Process(i, model));
				process.start();	
			}
			Thread notifier = new Thread(new Notifier(socket, model));
			notifier.start();

			Thread dispatcher = new Thread(new Dispatcher(socket, model));
			dispatcher.start();
		}
		catch (IOException e)
		{
			System.err.println("ServerSocket");
			e.printStackTrace();
			System.exit(-1);
		}
	}
}