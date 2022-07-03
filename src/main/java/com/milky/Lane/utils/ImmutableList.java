package com.milky.Lane.utils;

import java.util.ArrayList;
import java.util.List;

sealed public interface ImmutableList<E> {
    List<E> toList();
    boolean isEmpty();
    record Empty<E>() implements ImmutableList<E> {
        @Override
        public List<E> toList() {
            return new ArrayList<>();
        }

        @Override
        public boolean isEmpty() {
            return true;
        }
    }
    record Cons<E>(E e, ImmutableList<E> list) implements ImmutableList<E>{
        @Override
        public List<E> toList() {
            var curE = e;
            var curL = list;
            var res = new ArrayList<E>();
            while (curL instanceof Cons<E> cons) {
                res.add(curE);
                curL = cons.list;
                curE = cons.e;
            }
            res.add(curE);
            return res;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }
    }
}
