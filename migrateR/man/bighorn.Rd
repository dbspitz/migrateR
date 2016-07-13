\name{bighorn}
\alias{bighorn}
\title{GPS Monitoring of One Sierra Bighorn Sheep in California's Sierra Nevada Mountains}
\description{ This dataset contains GPS relocations of 1 female Sierra Nevada Bighorn sheep (Ovis canadensis sierrae) in California's' Sierra Nevada Mountains over four years. }
\usage{
data(bighorn)
}
\format{
This dataset is an object of class \code{\link{ltraj}}.
}
\details{
For convenience, the original dataset was subset to 1 location/day. Coordinates have been offset by a constant to protect this federally-endangered species. 
}
\source{
	Californa Department of Fish and Wildlife (Bishop, CA)
}
\examples{
  data(bighorn)
  bighorn
}
