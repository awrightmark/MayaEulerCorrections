# MayaEulerCorrections
Brief Description: An R script for correcting out-of-order Euler XYZ rotation coordinates caused by the 'jAx' function the Maya XROMM Shelf

When using the 'jAx' function from the Maya XROMM Shelf, poses that exceed 180 degrees along any rotational axis appear to change XYZ rotation order. I have not explored the extent of this issue or why it occurs within the 'jAx' function, but I have here created a simple R script to correct exported Translation/Rotation (TR) files with data computed using this function.

The basic premise of this script is that it converts Euler XYZ coordinates throughout a TR file into quaternion notation, which is independent of rotation order. It then converts quaternion notation back into Euler XYZ coordinates. The design provides seamless integration with existing Maya data processing workflow.

The script is commented as a tutorial with visual aids to, first, explain the problem, and then, to guide the user through applying it to their own TR .csv files.

Please reach out if you discover any errors. I have thus far only user-tested this with my research data and the provided sample data.
