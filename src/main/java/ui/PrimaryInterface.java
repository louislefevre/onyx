package ui;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

public final class PrimaryInterface extends Application
{
    public void launchInterface()
    {
        Application.launch();
    }

    @Override
    public void start(Stage primaryStage) throws Exception
    {
        Parent root = FXMLLoader.load(getClass().getResource("/main.fxml"));
        primaryStage.setTitle("Onyx Compiler");
        primaryStage.setScene(new Scene(root));
        primaryStage.show();

        //IdeScene ideScene = new IdeScene();
        //ideScene.setStage(primaryStage);
    }
}
