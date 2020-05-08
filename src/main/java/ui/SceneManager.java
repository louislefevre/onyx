package ui;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

import java.io.IOException;

public final class SceneManager extends Application
{
    public static void launchInterface()
    {
        Application.launch();
    }

    @Override
    public void start(Stage primaryStage) throws IOException
    {
        startMainStage(primaryStage);
    }

    void startMainStage(Stage primaryStage) throws IOException
    {
        setStage(primaryStage,"/main.fxml", "Onyx Compiler");
    }

    void startReplStage() throws IOException
    {
        setStage(new Stage(),"/repl.fxml", "Onyx REPL");
    }

    private void setStage(Stage stage, String path, String title) throws IOException
    {
        Parent root = FXMLLoader.load(getClass().getResource(path));
        stage.setTitle(title);
        stage.setScene(new Scene(root));
        stage.show();
    }
}
