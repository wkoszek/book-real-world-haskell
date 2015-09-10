/** snippet rootMeanSquare */
struct double_list 
{
    struct double_list *next;
    double val;
};
  
double rootMeanSquare(const struct double_list *list)
{
    double mean_square = 0;
    size_t length = 0;
    
    while (list != NULL) {
	mean_square += list->val * list->val;
	length++;
	list = list->next;
    }

    return sqrt(mean_square / length);
}
/** /snippet rootMeanSquare */
