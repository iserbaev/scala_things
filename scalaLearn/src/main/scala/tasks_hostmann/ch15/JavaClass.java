package tasks_hostmann.ch15;

import java.io.IOException;

/**
 * Created by ilnur on 19.01.17.
 */
public class JavaClass {
    public static void main(String[] args) {
        /**
         * 4
         */
        System.out.println(Main15.sum(5,8,9,6,6,3,2,5,6));

        /**
         * 5
         */
        try{
            System.out.println(Main15.readFile("/home/ilnur/Загрузки/opendj/README.txt"));
        } catch (IOException e){}
    }
}
