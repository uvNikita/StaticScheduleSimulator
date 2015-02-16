module Main (
    main
) where

import Graphics.QML(runEngineLoop, defaultEngineConfig, fileDocument, initialDocument)
import Paths_StaticScheduleSimulator (getDataFileName)

main = do
    qml <- getDataFileName "qml/window.qml"
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument qml
    }
