import CoverallsPlugin.CoverallsKeys._

instrumentSettings

CoverallsPlugin.coverallsSettings

ScoverageKeys.highlighting := true

sys.props.get("coveralls-repo-token").toList.map(coverallsToken := _)
