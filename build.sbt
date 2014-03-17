name := "calamp"

version := "0.1"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
  "com.google.guava" % "guava" % "15.0",
  "com.google.code.findbugs"    % "jsr305"                                      % "1.3.9"               % "test",
  "org.scalatest" 	% "scalatest_2.10" % "2.0.RC3" % "test",
  "junit" 		% "junit" 	   % "4.10"    % "test"
)