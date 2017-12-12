/*
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
*/

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;
import java.math.BigInteger;
import java.net.Socket;
import java.time.LocalDateTime;
import java.util.Random;

class Logger {
    static void LogToConsole(String logMessage) {
        System.out.println(LocalDateTime.now() + " - " + logMessage);
    }
}

enum State {
    Idle, Processing, Done
}

class Notifier implements Runnable {
    private Socket socket;
    private ProcessorsStateModel model;

    Notifier(Socket socket, ProcessorsStateModel model) {
        this.socket = socket;
        this.model = model;
    }

    @Override
    public void run() {
        try {
            OutputStream outStream = socket.getOutputStream();
            PrintWriter socketWriter = new PrintWriter(new OutputStreamWriter(outStream));

            int id = 0;
            while (true) {
                if (State.Done.equals(model.getState(id))) {
                    String output = "JobOut(" + model.getJobId(id) + "," + Integer.toString(id + 1) + "," + model.getGCD(id) + ")";        // Processors expected range [1..N]
                    Logger.LogToConsole("output " + output);
                    socketWriter.print(output + "\n");
                    socketWriter.flush();
                    model.setState(id, State.Idle);
                }

                id += 1;
                if (id == model.getNrOfProcessors())
                    id = 0;

                Thread.sleep(1);
            }
        } catch (IOException | InterruptedException e) {
            System.err.println("Notifier");
            e.printStackTrace();
            System.exit(-2);
        }
    }
}

class Dispatcher implements Runnable {
    private Socket socket;
    private ProcessorsStateModel model;

    Dispatcher(Socket socket, ProcessorsStateModel model) {
        this.socket = socket;
        this.model = model;
    }

    @Override
    public void run() {
        try {
            InputStream inputStream = socket.getInputStream();
            BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(inputStream));

            int id = 0;
            while (true) {
                String line = bufferedReader.readLine();
                if (line == null) {
                    System.exit(-101);
                }
                Logger.LogToConsole("input: " + line);

                boolean dispatched = false;
                while (!dispatched) {
                    if (State.Idle.equals(model.getState(id))) {
                        model.setData(id, line.substring(8, line.length() - 1));
                        model.setState(id, State.Processing);
                        dispatched = true;
                    }

                    id += 1;
                    if (id == model.getNrOfProcessors())
                        id = 0;

                    Thread.sleep(1);
                }
            }
        } catch (IOException | InterruptedException e) {
            System.err.println("Dispatcher");
            e.printStackTrace();
            System.exit(-3);
        }
    }
}

class Process implements Runnable {

    private int id;
    private ProcessorsStateModel model;

    public Process(int id, ProcessorsStateModel model) {
        this.id = id;
        this.model = model;
    }

    @Override
    public void run() {
        Random random = new Random();
        try {
            while (true) {
                while (!State.Processing.equals(model.getState(id))) {
                    Thread.sleep(100);
                }

                int fixSleepTime = 1000;
                int randSleepTime = 1000;
                int sleepTime = fixSleepTime + random.nextInt(randSleepTime);
                Logger.LogToConsole("Processor " + this.id + ": Sleeping for " + sleepTime + "ms...");
                Thread.sleep(sleepTime);
                model.setState(id, State.Done);
            }
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}

class ProcessorsStateModel {
    private static String gcd(String a, String b) {
        BigInteger b1 = new BigInteger(a);
        BigInteger b2 = new BigInteger(b);
        BigInteger gcd = b1.gcd(b2);
        return gcd.toString();
    }

    private int nrOfProcessors;

    public int getNrOfProcessors() {
        return nrOfProcessors;
    }

    private State states[];

    public State getState(int processorIndex) {
        return states[processorIndex];
    }

    public void setState(int processorIndex, State state) {
        states[processorIndex] = state;
    }

    protected String data[][];

    public String getJobId(int i) {
        return data[i][0];
    }

    public String getGCD(int i) {
        return data[i][4];
    }

    ProcessorsStateModel(int nrOfProcessors) {
        this.nrOfProcessors = nrOfProcessors;
        states = new State[nrOfProcessors];
        for (int i = 0; i < nrOfProcessors; i++) {
            states[i] = State.Idle;
            assert State.Idle.equals(states[i]) : "initial state is Idle";
        }
        data = new String[nrOfProcessors][5];
    }

    public void setData(int id, String line) {
        String[] values = line.split(",");
        assert values.length == 4 : "Wrong input";
        data[id][0] = values[0];
        data[id][1] = values[1];
        data[id][2] = values[2];
        data[id][3] = values[3];
        data[id][4] = gcd(values[2], values[3]);
    }
}

class ProcessorsStateView extends JPanel implements ActionListener {

    private static final long serialVersionUID = 8547665121133548210L;
    private final int sideLengthPx = 200;
    private final int border = 20;
    private final ProcessorsStateModel psm;
    private static Timer timer;

    ProcessorsStateView(ProcessorsStateModel psm) {
        this.psm = psm;
        int delay = 100;
        timer = new Timer(delay, this);
        timer.start();
        setPreferredSize(new Dimension(border + psm.getNrOfProcessors() * (border + sideLengthPx),
                2 * border + sideLengthPx));
    }

    public void paintComponent(Graphics g) {
        super.paintComponent(g);
        for (int i = 0; i < psm.getNrOfProcessors(); i++) {
            g.setColor(psm.getState(i).equals(State.Idle) ? Color.GREEN : Color.RED);
            g.fillRect(border + (border + sideLengthPx) * i, border, sideLengthPx, sideLengthPx);
            if (psm.getState(i).equals(State.Processing)) {
                g.setColor(Color.WHITE);
                g.setFont(getFont().deriveFont(44.0f));
                g.drawString(psm.getJobId(i),
                        border + (border + sideLengthPx) * i + (sideLengthPx - g.getFontMetrics().stringWidth(psm.getJobId(i))) / 2,
                        border + (sideLengthPx + g.getFontMetrics().getHeight()) / 2);
            }
        }
    }

    @Override
    public void actionPerformed(ActionEvent arg0) {
        repaint();
    }
}
