package tasks_hostmann.ch1_8;

/**
 * Created by ilnur on 17.11.16.
 */
public class CarJava {
    private String manufacturer;
    private String model;
    private String regNum;
    private int year=-1;

    public CarJava(String manufacturer, String model) {
        this.manufacturer = manufacturer;
        this.model = model;
    }

    public CarJava(String manufacturer, String model, String regNum) {
        this.manufacturer = manufacturer;
        this.model = model;
        this.regNum = regNum;
    }

    public CarJava(String manufacturer, String model, int year) {
        this.manufacturer = manufacturer;
        this.model = model;
        this.year = year;
    }

    public CarJava(String manufacturer, String model, String regNum, int year) {
        this.manufacturer = manufacturer;
        this.model = model;
        this.regNum = regNum;
        this.year = year;
    }

    public String getManufacturer() {
        return manufacturer;
    }

    public String getModel() {
        return model;
    }

    public String getRegNum() {
        return regNum;
    }

    public int getYear() {
        return year;
    }

    public void setRegNum(String regNum) {
        this.regNum = regNum;
    }
}
