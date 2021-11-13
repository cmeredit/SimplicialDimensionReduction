import org.joml.Matrix4f;
import org.joml.Vector3f;
import org.lwjgl.*;
import org.lwjgl.glfw.*;
import org.lwjgl.opengl.*;
import org.lwjgl.system.*;

import java.nio.*;

import static java.lang.Math.sin;
import static org.lwjgl.glfw.Callbacks.*;
import static org.lwjgl.glfw.GLFW.*;
import static org.lwjgl.opengl.GL11.*;
import static org.lwjgl.opengl.GL20.*;
import static org.lwjgl.system.MemoryStack.*;
import static org.lwjgl.system.MemoryUtil.*;

import ScalaComp.Main;
import GLUtil.Camera;
import GLUtil.*;

public class HelloWorld {

    // The window handle
    private long window;

    // Matrix for the projection-view-model transform and its buffer
    FloatBuffer pvmFloatBuffer;
    Matrix4f pvmMatrix;

    public void run() {
        System.out.println("Hello LWJGL " + Version.getVersion() + "!");

        init();
        loop();

        // Free the window callbacks and destroy the window
        glfwFreeCallbacks(window);
        glfwDestroyWindow(window);

        // Terminate GLFW and free the error callback
        glfwTerminate();
        glfwSetErrorCallback(null).free();
    }

    private void init() {
        // Setup an error callback. The default implementation
        // will print the error message in System.err.
        GLFWErrorCallback.createPrint(System.err).set();

        // Initialize GLFW. Most GLFW functions will not work before doing this.
        if ( !glfwInit() )
            throw new IllegalStateException("Unable to initialize GLFW");

        // Configure GLFW
        glfwDefaultWindowHints(); // optional, the current window hints are already the default
        glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE); // the window will stay hidden after creation
        glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE); // the window will be resizable

        // Create the window
        window = glfwCreateWindow(640, 480, "Simp", NULL, NULL);
        if ( window == NULL )
            throw new RuntimeException("Failed to create the GLFW window");

        // Setup a key callback. It will be called every time a key is pressed, repeated or released.
        glfwSetKeyCallback(window, (window, key, scancode, action, mods) -> {
            if ( key == GLFW_KEY_ESCAPE && action == GLFW_RELEASE )
                glfwSetWindowShouldClose(window, true); // We will detect this in the rendering loop
        });

        // Get the thread stack and push a new frame
        try ( MemoryStack stack = stackPush() ) {
            IntBuffer pWidth = stack.mallocInt(1); // int*
            IntBuffer pHeight = stack.mallocInt(1); // int*

            // Get the window size passed to glfwCreateWindow
            glfwGetWindowSize(window, pWidth, pHeight);

            // Get the resolution of the primary monitor
            GLFWVidMode vidmode = glfwGetVideoMode(glfwGetPrimaryMonitor());

            // Center the window
            glfwSetWindowPos(
                    window,
                    1920 + (vidmode.width() - pWidth.get(0)) / 2,
                    (vidmode.height() - pHeight.get(0)) / 2
            );
            System.out.println(vidmode.width());
            System.out.println(pWidth.get(0));
        } // the stack frame is popped automatically

        // Make the OpenGL context current
        glfwMakeContextCurrent(window);
        // Enable v-sync
        glfwSwapInterval(1);

        // Make the window visible
        glfwShowWindow(window);

        pvmFloatBuffer = MemoryUtil.memAllocFloat(16);
        pvmMatrix = new Matrix4f();
    }

    private void loop() {
        // This line is critical for LWJGL's interoperation with GLFW's
        // OpenGL context, or any context that is managed externally.
        // LWJGL detects the context that is current in the current thread,
        // creates the GLCapabilities instance and makes the OpenGL
        // bindings available for use.
        GL.createCapabilities();

        float[] vertices = {
                0.5f,  0.5f, 0.0f,  // top right
                0.5f, -0.5f, 0.0f,  // bottom right
                -0.5f, -0.5f, 0.0f,  // bottom left
                -0.5f,  0.5f, 0.0f   // top left
        };
        int[] indices = {
                0, 1, 3,   // first triangle
                1, 2, 3    // second triangle
        };
        FloatBuffer buffer = BufferUtils.createFloatBuffer(vertices.length);
        buffer.put(vertices);
        buffer.flip();

        int vao = GL30.glGenVertexArrays();
        int vbo = GL15.glGenBuffers();
        int indicesVBO = GL15.glGenBuffers();

        GL30.glBindVertexArray(vao);

        GL15.glBindBuffer(GL15.GL_ARRAY_BUFFER, vbo);
        GL30.glBufferData(GL15.GL_ARRAY_BUFFER, buffer, GL15.GL_STATIC_DRAW);
        GL30.glVertexAttribPointer(0, 3, GL_FLOAT, false, 0, 0);
        GL30.glEnableVertexAttribArray(0);
        GL30.glBindBuffer(GL15.GL_ARRAY_BUFFER,0);

        GL15.glBindBuffer(GL15.GL_ELEMENT_ARRAY_BUFFER, indicesVBO);
        // Actually store the indices
        IntBuffer indicesBuffer = BufferUtils.createIntBuffer(indices.length);
        indicesBuffer.put(indices);
        indicesBuffer.flip();
        GL15.glBufferData(GL15.GL_ELEMENT_ARRAY_BUFFER, indicesBuffer, GL15.GL_STATIC_DRAW);

        GL30.glBindVertexArray(0);

        // Set up our shader program...
        String vertexShaderSource = """
                #version 330 core
                layout (location = 0) in vec3 aPos;
                
                uniform mat4 pvmMatrix;
                
                void main()
                {
                   gl_Position = pvmMatrix * vec4(aPos.x, aPos.y, aPos.z, 1.0);
                }\0""";
        int vertexShader = GL30.glCreateShader(GL20.GL_VERTEX_SHADER);
        GL30.glShaderSource(vertexShader, vertexShaderSource);
        GL30.glCompileShader(vertexShader);

        String fragmentShaderSource = """
                #version 330 core
                out vec4 FragColor;
                
                uniform vec4 uniformColor;
                                
                void main()
                {
                    FragColor = uniformColor;
                }\0""";
        int fragmentShader = GL30.glCreateShader(GL20.GL_FRAGMENT_SHADER);
        GL30.glShaderSource(fragmentShader, fragmentShaderSource);
        GL30.glCompileShader(fragmentShader);

        int shaderProgram = GL30.glCreateProgram();
        GL30.glAttachShader(shaderProgram, vertexShader);
        GL30.glAttachShader(shaderProgram, fragmentShader);
        GL30.glLinkProgram(shaderProgram);

        GL30.glDeleteShader(vertexShader);
        GL30.glDeleteShader(fragmentShader);


        // Mesh m = MeshLoader.createMesh(vertices, indices);

        // Set the clear color
        glViewport((640-480/2)/2, 480/4, 480/2, 480/2);
        glScissor((640-480/2)/2, 480/4, 480/2, 480/2);
        glEnable(GL_SCISSOR_TEST);
        // glScissor?

        //glEnable(GL_SCISSOR_TEST);

        Camera cameraTest = new Camera(
            new Vector3f(0.0f, 0.0f, 5.0f), // Position

            new Vector3f(0.0f, 1.0f, 0.0f), // Up
            new Vector3f(0.0f, 0.0f, -1.0f), // Front

            2.5f,
            0.1f,

            -90.0f,
            0.0f,

            45.0f
        );


        // Run the rendering loop until the user has attempted to close
        // the window or has pressed the ESCAPE key.
        while ( !glfwWindowShouldClose(window) ) {
            //glScissor(0, 0, 10, 10);
            //glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); // clear the framebuffer

            // "Restrict" to entire screen
            glViewport(0, 0, 640, 480);
            glScissor(0, 0, 640, 480);
            glClearColor(0.1f, 0.1f, 0.15f, 1.0f);
            glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

            // Restrict to center viewport
            int w = 480*2/3;
            glViewport((640-w)/2, (480-w)/2, w, w);
            glScissor((640-w)/2, (480-w)/2, w, w);
            glClearColor(0.3f, 0.3f, 0.3f, 1.0f);
            glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

            // Draw the rectangle
            GL30.glUseProgram(shaderProgram);
            double timeValue = glfwGetTime();
            double greenValue = (sin(timeValue) / 2.0f) + 0.5f;
            int vertexColorLocation = glGetUniformLocation(shaderProgram, "uniformColor");
            glUniform4f(vertexColorLocation, 0.0f, (float) greenValue, 0.0f, 1.0f);

//            pvmMatrix.identity()
//                            .perspective((float) Math.toRadians(45.0f), 1.0f, 0.01f, 100.0f)
//                                    .lookAt(
//                                            0.0f, 0.0f, 10.0f * (float) (sin(timeValue) + 1.1),
//                                            0.0f, 0.0f, 0.0f,
//                                            0.0f, 1.0f, 0.0f
//                                    );

            cameraTest.processKeyboard(Forward$.MODULE$, -0.1f);

            pvmMatrix.identity();
            pvmMatrix.perspective((float) Math.toRadians(45.0f), 1.0f, 0.1f, 100.0f);
            pvmMatrix.mul(cameraTest.getViewMatrix());
            pvmMatrix.rotate((float) Math.toRadians(45.0), 0.0f, 0.0f, 1.0f);



            int cameraMatrixLocation = glGetUniformLocation(shaderProgram, "pvmMatrix");
            glUniformMatrix4fv(cameraMatrixLocation, false, pvmMatrix.get(pvmFloatBuffer));

            GL30.glBindVertexArray(vao);
            GL30.glDrawElements(GL_TRIANGLES, vertices.length, GL_UNSIGNED_INT, 0);
            GL30.glBindVertexArray(0);

//            // Draw our triangle! -----------------------------------------------------------------------------
//            GL30.glBindVertexArray(m.getVaoID());
//            GL20.glEnableVertexAttribArray(0);
//
//            GL11.glDrawElements(GL_TRIANGLES, m.getNumVertices(), GL_UNSIGNED_INT, 0);
//
//            GL20.glDisableVertexAttribArray(0);
//            GL30.glBindVertexArray(0);
//            // ------------------------------------------------------------------------------------------------
//
//
//
//            glViewport(150, 150, 150, 150);
//
//            // Draw our triangle! -----------------------------------------------------------------------------
//            GL30.glBindVertexArray(m.getVaoID());
//            GL20.glEnableVertexAttribArray(0);
//
//            GL11.glDrawElements(GL_TRIANGLES, m.getNumVertices(), GL_UNSIGNED_INT, 0);
//
//            GL20.glDisableVertexAttribArray(0);
//            GL30.glBindVertexArray(0);
//            // ------------------------------------------------------------------------------------------------


            glfwSwapBuffers(window); // swap the color buffers

            // Poll for window events. The key callback above will only be
            // invoked during this call.
            glfwPollEvents();
        }
    }

    public static void main(String[] args) {
        Main.run();
        new GUIMain.Main().run();
    }

}
