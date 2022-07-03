package com.milky.Lane.exception;

public class LaneException extends Exception {
    public final String message;

    public LaneException(String message) {
        this.message = message;
    }
}
