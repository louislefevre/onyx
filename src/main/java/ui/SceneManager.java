package ui;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.geometry.Pos;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
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

    void startPopupStage(String message)
    {
        VBox layout= new VBox();
        Label messageLabel = new Label(message);
        Button closeButton= new Button("Close");
        Scene scene = new Scene(layout, 200, 100);
        Stage window = new Stage();

        layout.getChildren().addAll(messageLabel, closeButton);
        layout.setAlignment(Pos.CENTER);
        closeButton.setOnAction(e -> window.close());

        window.setTitle("Error");
        window.initModality(Modality.APPLICATION_MODAL);
        window.setScene(scene);
        window.showAndWait();
    }

    private void setStage(Stage stage, String path, String title) throws IOException
    {
        Parent root = FXMLLoader.load(getClass().getResource(path));
        stage.setTitle(title);
        stage.setScene(new Scene(root));
        stage.show();
    }
}
