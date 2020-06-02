import javafx.application.Application;
import javafx.stage.Stage;
import ui.StageManager;

import java.io.IOException;

public class Main extends Application
{
    public static void main(String[] args)
    {
        Application.launch();
    }

    @Override
    public void start(Stage primaryStage) throws IOException
    {
        StageManager stageManager = new StageManager();
        stageManager.startMainStage(primaryStage);
    }
}

// VM Options: --module-path lib/openjfx-11.0.2_linux-x64_bin-sdk/javafx-sdk-11.0.2/lib --add-modules javafx.controls,javafx.fxml