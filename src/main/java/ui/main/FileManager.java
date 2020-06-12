package ui.main;

import javafx.stage.FileChooser;
import javafx.stage.Stage;
import lombok.Getter;
import lombok.Setter;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.AccessDeniedException;
import java.nio.file.Files;

/**
 * The FileManager class is used for managing files.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
@Getter
@Setter
final class FileManager
{
    private File initialDirectory;
    private String[] validFileExtensions;
    private File currentFile;
    private String originalText;

    /**
     * Constructs a FileManager object initialised with an initial directory and defined valid file extensions.
     * <p>
     * The initial directory is the location the user is first taken to when opening a FileChooser window.
     * <p>
     * The valid file extensions provide a list of file types which the user is allowed to open. Note that provided
     * Strings must not include the initial period, as this is applied already.
     *
     * @param initialDirectory The initial directory presented to the user when opening a FileChooser window
     * @param validFileExtensions A list of valid file types the user can open or save as.
     */
    public FileManager(File initialDirectory, String... validFileExtensions)
    {
        this.initialDirectory = initialDirectory;
        this.validFileExtensions = validFileExtensions;
        this.currentFile = null;
        this.originalText = "";
    }

    /**
     * Constructs a FileManager object initialised with a default directory and file extensions.
     * <p>
     * This method sets the initialDirectory to the users 'documents' folder, and allows txt file extensions.
     */
    public FileManager()
    {
        this(new File(System.getProperty("user.home"), "/Documents"), "txt");
    }

    /**
     * Creates a new file by returning an empty string.
     *
     * @return An empty String
     */
    public String openNewFile()
    {
        return newFile();
    }

    /**
     * Opens a FileChooser window, allowing the user to open a file.
     * <p>
     * When the user has chosen a file, its contents will be returned by this method.
     * <p>
     * The FileChooser will initially open the folder defined by the initialDirectory field.
     * Furthermore, the user will only be able to selected files defined in the validFileExtensions list.
     *
     * @return A String containing the contents of the file
     * @throws IOException If the selected file doesn't exist, or the user does not have permission to read the file
     * @throws NullPointerException If no file was selected
     * @throws IllegalArgumentException If the file extension was invalid
     */
    public String openFile() throws IOException, NullPointerException, IllegalArgumentException
    {
        FileChooser fileChooser = createFileChooser();

        File file = fileChooser.showOpenDialog(new Stage());
        if (file == null)
            throw new NullPointerException("No file was selected");
        else if (isNonExistentFile(file))
            throw new FileNotFoundException("Selected file doesn't exist: " + file.getPath());
        else if (isInvalidReadFile(file))
            throw new AccessDeniedException("Access denied - you do not have permission to read this file");
        else if (isInvalidFileExtension(file))
            throw new IllegalArgumentException("Invalid file extension");
        else
            return readFile(file);
    }

    /**
     * Opens a FileChooser window, allowing the user to name and save a file.
     * <p>
     * The user will only be able to save a file with an extension defined in the validFileExtensions list.
     * <p>
     * This method sets the initial file name to "new-script.txt" by default.
     *
     * @param text The text to be written to the file
     * @throws IOException If the file could not be written to
     * @throws NullPointerException If no file was selected
     * @throws IllegalArgumentException If the file extension was invalid
     */
    public void saveFileAs(String text) throws IOException, NullPointerException, IllegalArgumentException
    {
        String initialFileName = "new-script.txt";
        saveFileAs(text, initialFileName);
    }

    /**
     * Opens a FileChooser window with a specified initial file name, allowing the user to name and save a file.
     * <p>
     * The user will only be able to save a file with an extension defined in the validFileExtensions list.
     *
     * @param text The text to be written to the file
     * @param initialFileName The initial file name of the file being saved
     * @throws IOException If the file could not be written to
     * @throws NullPointerException If no file was selected
     * @throws IllegalArgumentException If the file extension was invalid
     */
    public void saveFileAs(String text, String initialFileName) throws IOException, NullPointerException, IllegalArgumentException
    {
        FileChooser fileChooser = createFileChooser();
        fileChooser.setInitialFileName(initialFileName);

        File file = fileChooser.showSaveDialog(new Stage());
        if (file == null)
            throw new NullPointerException("No file was selected");
        else if (isInvalidFileExtension(file))
            throw new IllegalArgumentException("Invalid file extension");
        else
            writeFile(file, text);
    }

    /**
     * Saves a file without opening a FileChooser window.
     * <p>
     * This method will only run if the current file has not been modified in any way, such as the file being moved,
     * renamed, deleted, or updated. If this is the case, the saveFileAs method will run instead.
     *
     * @param text The text to be written to the file
     * @throws IOException If the file could not be written to
     * @throws NullPointerException If no file was selected
     * @throws IllegalArgumentException If the file extension was invalid
     */
    public void saveFile(String text) throws IOException, NullPointerException, IllegalArgumentException
    {
        File file = currentFile;
        if (isValidSaveFile(file))
            writeFile(file, text);
        else
            saveFileAs(text);
    }

    /**
     * Returns a boolean indicating if a file has been saved.
     * <p>
     * The input text is compared against the last saved or opened file to see if their contents match. If they do,
     * the file must have been saved.
     *
     * @param text The text being checked to see if its been saved
     * @return A boolean indicating whether the text has been saved or not
     */
    public boolean checkIfSaved(String text)
    {
        return originalText.equals(text);
    }

    private FileChooser createFileChooser()
    {
        FileChooser fileChooser = new FileChooser();
        addInitialDirectory(fileChooser);
        addExtensionFilters(fileChooser);
        return fileChooser;
    }

    private void addInitialDirectory(FileChooser fileChooser)
    {
        if (initialDirectory.exists())
            fileChooser.setInitialDirectory(initialDirectory);
        else
            System.out.println("Failed to find initial directory: " + initialDirectory);
    }

    private void addExtensionFilters(FileChooser fileChooser)
    {
        for (String extension : validFileExtensions)
        {
            fileChooser.getExtensionFilters().add(
                    new FileChooser.ExtensionFilter(extension, "*." + extension)
            );
        }
    }

    private String newFile()
    {
        String text = "";
        currentFile = null;
        originalText = text;
        return text;
    }

    private String readFile(File file) throws IOException
    {
        String fileText = Files.readString(file.toPath());
        currentFile = file;
        originalText = fileText;
        return fileText;
    }

    private void writeFile(File file, String text) throws FileNotFoundException
    {
        PrintWriter writer = new PrintWriter(file);
        writer.print(text);
        writer.close();
        currentFile = file;
        originalText = text;
    }

    private boolean isNonExistentFile(File file)
    {
        return !file.exists();
    }

    private boolean isInvalidReadFile(File file)
    {
        return !file.canRead();
    }

    private boolean isInvalidWriteFile(File file)
    {
        return !file.canWrite();
    }

    private boolean isValidSaveFile(File file)
    {
        return file != null && file.exists() && file.isFile() && file.canWrite() && isValidFileExtension(file);
    }

    private boolean isInvalidFileExtension(File file)
    {
        return !isValidFileExtension(file);
    }

    private boolean isValidFileExtension(File file)
    {
        String extension = getFileExtension(file);
        for (String ext : validFileExtensions)
            if (extension.equals(ext))
                return true;
        return false;
    }

    private String getFileExtension(File file)
    {
        String name = file.getName();
        int lastIndexOf = name.lastIndexOf(".");
        if (lastIndexOf == -1)
            return "";

        return name.substring(lastIndexOf + 1);
    }
}
