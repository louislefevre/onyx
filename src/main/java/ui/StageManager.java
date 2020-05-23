package ui;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

import java.io.IOException;

public final class StageManager extends Application
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
        String title = "Onyx Compiler";
        String path = "/main.fxml";

        Parent root = FXMLLoader.load(getClass().getResource(path));
        Scene scene = new Scene(root);
        scene.getStylesheets().add(StageManager.class.getResource("/syntax.css").toExternalForm());

        primaryStage.setTitle(title);
        primaryStage.setScene(scene);
        primaryStage.show();
    }

    void startReplStage() throws IOException
    {
        String title = "Onyx REPL";
        String path = "/repl.fxml";

        Parent root = FXMLLoader.load(getClass().getResource(path));
        Scene scene = new Scene(root);

        Stage stage = new Stage();
        stage.setTitle(title);
        stage.setScene(scene);
        stage.show();
    }
}
