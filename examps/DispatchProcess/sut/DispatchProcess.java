/*
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
*/

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;

import javax.swing.*;

public class DispatchProcess {

    protected static final int nrOfProcessors = 4;

    private static void createAndShowGUI(ProcessorsStateModel model) {
        //Create and set up the window.
        JFrame frame = new JFrame("Process");
        frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);

        ProcessorsStateView view = new ProcessorsStateView(model);
        frame.add(view);

        //Display the window.
        frame.pack();
        frame.setVisible(true);
        Logger.LogToConsole("UI ready");
    }

    public static void main(String[] args) {
        ProcessorsStateModel model = new ProcessorsStateModel(nrOfProcessors);
        Initialize(model);
    }

    protected static void Initialize(ProcessorsStateModel model) {
        try {
            //Schedule a job for the event-dispatching thread:
            //creating and showing this application's GUI.
            SwingUtilities.invokeLater(() -> createAndShowGUI(model));

            ServerSocket serverSocket = new ServerSocket(7890);
            Logger.LogToConsole("Waiting for tester");
            Socket socket = serverSocket.accept();
            Logger.LogToConsole("Tester connected");

            Logger.LogToConsole("Starting processors...");
            for (int i = 0; i < model.getNrOfProcessors(); i++) {
                Thread process = new Thread(new Process(i, model));
                process.start();
            }
            Logger.LogToConsole("Starting notifier...");
            Thread notifier = new Thread(new Notifier(socket, model));
            notifier.start();

            Logger.LogToConsole("Starting dispatcher...");
            Thread dispatcher = new Thread(new Dispatcher(socket, model));
            dispatcher.start();

            Logger.LogToConsole("Ready");
        } catch (IOException e) {
            System.err.println("Exception in ServerSocket");
            e.printStackTrace();
            System.exit(-1);
        }
    }
}
