CMake
=====

## Install Dirs

```
https://cmake.org/cmake/help/latest/module/GNUInstallDirs.html
```

## Create RPM from CMake

```
Step 1: Enable CPack in Your CMake Project

Add the following lines to your CMakeLists.txt file to enable CPack and configure it for RPM generation:

include(CPack)

set(CPACK_GENERATOR "RPM")
set(CPACK_PACKAGE_NAME "YourPackageName")
set(CPACK_PACKAGE_VERSION "1.0.0")
set(CPACK_PACKAGE_RELEASE "1")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Your package description")
set(CPACK_PACKAGE_VENDOR "YourVendorName")
set(CPACK_RPM_PACKAGE_LICENSE "MIT")
set(CPACK_RPM_PACKAGE_GROUP "Development/Tools")
set(CPACK_RPM_PACKAGE_ARCHITECTURE "x86_64") # Adjust as needed
set(CPACK_PACKAGING_INSTALL_PREFIX "/usr")

# Include additional files or dependencies if necessary
install(TARGETS your_target DESTINATION bin)

Step 2: Build the Project

Run the following commands to build your project:

$ mkdir build && cd build
$ cmake
$ make

Step 3: Generate the RPM Package

After building the project, use cpack to create the RPM package:

$ cpack -G RPM

This will generate an .rpm file in the build directory.

Step 4: Verify the RPM

To inspect the contents of the generated RPM, use:

$ rpm -qpl YourPackageName-1.0.0-1.x86_64.rpm

Best Practices

Use CPACK_RPM_PACKAGE_REQUIRES to specify dependencies:

set(CPACK_RPM_PACKAGE_REQUIRES "dependency1 >= 1.0, dependency2 >= 2.0")

Ensure all installed files are included in the package using install() commands in your CMakeLists.txt.
Test the RPM installation on a clean system or container.
This approach ensures a clean and portable way to create RPM packages directly from your CMake project.
```
