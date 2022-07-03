package com.milky.Lane.exception;

public class LaneBug extends LaneException {
    public LaneBug(String message) {
        super("Bug!!! " + message);
    }
}
