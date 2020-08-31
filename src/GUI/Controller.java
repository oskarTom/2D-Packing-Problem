package GUI;

import javafx.fxml.FXML;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

public class Controller {

    @FXML
    public void run(){
        String path = "C:\\Users\\tomos\\Documents\\Projects\\Code\\Navier-Stokes\\src\\core\\a.exe";
        try {
            Process p = Runtime.getRuntime().exec(path);
            InputStream is = p.getInputStream();
            InputStreamReader isr = new InputStreamReader(is);
            BufferedReader br = new BufferedReader(isr);
            String line;

            while ((line = br.readLine()) != null) {
                //screen.appendText(line+"\n");
            }
        } catch (IOException e) {
            System.out.println("Could not launch...");
            e.printStackTrace();
        }
    }
}
