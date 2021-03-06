package com.galois.cryptol.client.connection;

import java.lang.Runtime.*;
import java.util.*;
import java.io.*;
import java.util.function.*;

public class ConnectionManager<A> implements AutoCloseable {

    private final PipeFactory<A> newPipe;
    private final ProcessBuilder builder;

    private volatile Process process;
    private volatile Pipe<A> currentPipe;
    private volatile boolean closed = false;

    @FunctionalInterface
    public static interface PipeFactory<A> {
        public Pipe<A> make(OutputStream in, InputStream out, InputStream err)
            throws IOException;
    }

    private void destroyProcess() {
        if (process != null && !closed) {
            // System.err.println("Destroying process: " + process.pid());
            process.destroy();
            if (process.isAlive()) {
                process.destroyForcibly();
            }
            process = null;
        }
    }

    public synchronized Pipe<A> get() throws IOException {
        if (!closed) {
            // Destroy the old process
            destroyProcess();
            // Create the new process
            process = builder.start();
            // System.err.println("Created process: " + process.pid());
            var in  = process.getOutputStream();
            var out = process.getInputStream();
            var err = process.getErrorStream();
            return currentPipe = newPipe.make(in, out, err);
        } else {
            throw new IllegalStateException("Connection manager is closed");
        }
    }

    public ConnectionManager(ProcessBuilder builder, PipeFactory<A> newPipe)
        throws IOException {
        this.newPipe = newPipe;
        this.builder = builder;
        // Ensure that the current process gets killed with the JVM process
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
                    try {
                        this.close();
                    } catch (IOException e) {
                        throw new UncheckedIOException(e);
                    }
        }));
    }

    @Override
    public synchronized void close() throws IOException {
        if (!closed) {
            if (currentPipe != null) {
                currentPipe.close();
            }
            currentPipe = null;
            destroyProcess();
            closed = true;
        }
    }
}
