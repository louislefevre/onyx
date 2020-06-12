package ui;

import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

import java.io.IOException;

/**
 * The StageManager class is used setup and run each GUI.
 * <p>
 * Each Stage object is defined by its title, FXML file, and (optionally) stylesheet. The methods can then be called
 * to instantly generate a separate window.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
public final class StageManager
{
    /**
     * Start the main GUI.
     * <p>
     * The title, path, and stylesheet are hardcoded into this method.
     *
     * @param stage The stage which the interface will be displayed on
     * @throws IOException If the path is not found
     */
    public void startMain(Stage stage) throws IOException
    {
        String title = "Onyx Compiler";
        String path = "/fxml/main.fxml";
        String styleSheet = "/css/syntax.css";
        showStage(stage, title, path, styleSheet);
    }

    /**
     * Start the REPL GUI.
     * <p>
     * The title and path are hardcoded into this method.
     *
     * @param stage The stage which the interface will be displayed on
     * @throws IOException If the path is not found
     */
    public void startRepl(Stage stage) throws IOException
    {
        String title = "Onyx REPL";
        String path = "/fxml/repl.fxml";
        showStage(stage, title, path, null);
    }

    private void showStage(Stage stage, String title, String path, String styleSheet) throws IOException
    {
        Parent root = FXMLLoader.load(getClass().getResource(path));
        Scene scene = new Scene(root);

        if (styleSheet != null)
            scene.getStylesheets().add(StageManager.class.getResource(styleSheet).toExternalForm());

        stage.setTitle(title);
        stage.setScene(scene);
        stage.show();
    }
}
