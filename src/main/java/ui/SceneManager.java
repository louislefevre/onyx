package ui;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.geometry.Pos;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Label;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;

import java.io.IOException;
import java.util.concurrent.atomic.AtomicBoolean;

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
        String title = "Onyx Compiler";
        String path = "/main.fxml";

        Parent root = FXMLLoader.load(getClass().getResource(path));
        Scene scene = new Scene(root);
        scene.getStylesheets().add(SceneManager.class.getResource("/syntax.css").toExternalForm());

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

    void startPopupStage(String message)
    {
        VBox layout = new VBox();
        Label messageLabel = new Label(message);
        Button closeButton = new Button("Close");
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

    boolean startConfirmationWindow(String message)
    {
        Alert alert = new Alert(Alert.AlertType.CONFIRMATION);
        alert.setTitle("Confirmation");
        alert.setContentText(message);

        ButtonType yesButton = new ButtonType("Yes", ButtonBar.ButtonData.YES);
        ButtonType noButton = new ButtonType("No", ButtonBar.ButtonData.NO);
        ButtonType cancelButton = new ButtonType("Cancel", ButtonBar.ButtonData.CANCEL_CLOSE);
        alert.getButtonTypes().setAll(yesButton, noButton, cancelButton);

        AtomicBoolean confirm = new AtomicBoolean(false);
        alert.showAndWait().ifPresent(type -> {
            if (type.getButtonData() == ButtonBar.ButtonData.YES)
                confirm.set(true);
        });

        alert.close();
        return confirm.get();
    }
}
