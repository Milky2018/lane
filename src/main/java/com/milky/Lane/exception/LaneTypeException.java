package com.milky.Lane.exception;

public class LaneTypeException extends LaneException{
    public LaneTypeException(String message) {
        super("Type error: " + message);
    }
}
