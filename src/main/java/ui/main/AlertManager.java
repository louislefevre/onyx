package ui.main;

import javafx.scene.control.Alert;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.ButtonType;

import java.util.concurrent.atomic.AtomicBoolean;

/**
 * The AlertManager class is used setup and run alert messages.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
final class AlertManager
{
    /**
     * Start and open an error alert window.
     * <p>
     * This alert only provides the 'cancel' option.
     *
     * @param message The message to be displayed to the user
     */
    public void startErrorAlert(String message)
    {
        Alert alert = new Alert(Alert.AlertType.ERROR);
        alert.setTitle("Error");
        alert.setContentText(message);

        ButtonType cancelButton = new ButtonType("Close", ButtonBar.ButtonData.CANCEL_CLOSE);
        alert.getButtonTypes().setAll(cancelButton);

        alert.showAndWait().ifPresent(type -> {
            if (type.getButtonData() == ButtonBar.ButtonData.CANCEL_CLOSE)
                alert.close();
        });
    }

    /**
     * Start and open a confirmation window.
     * <p>
     * This alert provides the user with a choice between 'yes', 'no', and 'cancel'.
     * If true, the user chose the option 'yes'. If false, they chose the option 'no' or 'cancel'
     *
     * @param message The message to be displayed to the user
     * @return The users confirmation choice
     */
    public boolean startConfirmationAlert(String message)
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
