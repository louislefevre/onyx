package ui;

import javafx.application.Application;
import javafx.stage.Stage;

public final class PrimaryInterface extends Application
{
    public void launchInterface()
    {
        Application.launch();
    }

    @Override
    public void start(Stage primaryStage)
    {
        IdeScene ideScene = new IdeScene();
        ideScene.setStage(primaryStage);
    }
}
