void RSIc (double *gain, double *AvgGain, double *loss, double *AvgLoss, 
		   double *x, double *order, int *n)
{
	for ( int i = 1; i < (*n); i++ ) {
		AvgGain[i] = (AvgGain[i - 1] * ((*order) - 1) + gain[i]) / (*order);
		AvgLoss[i] = (AvgLoss[i - 1] * ((*order) - 1) + loss[i]) / (*order);
	}
}