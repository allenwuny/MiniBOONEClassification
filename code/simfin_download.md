~~~python
import simfin as sf
from IPython.display import clear_output

# Set your API-key for downloading data. This key gets the free data.
sf.set_api_key('free') 
# Set the local directory where data-files are stored.
# The directory will be created if it does not already exist.
sf.set_data_dir('./stocks/')

sf.load_income(variant='quarterly', market='us')
sf.load_income_banks(variant='quarterly', market='us')
sf.load_income_insurance(variant='quarterly', market='us')

sf.load_balance(variant='quarterly', market='us')
sf.load_balance_banks(variant='quarterly', market='us')
sf.load_balance_insurance(variant='quarterly', market='us')

sf.load_cashflow(variant='quarterly', market='us')
sf.load_cashflow_banks(variant='quarterly', market='us')
sf.load_cashflow_insurance(variant='quarterly', market='us')

sf.load_shareprices(variant='daily', market='us')
sf.load_companies(market='us')
sf.load_industries()

clear_output()
print('Data download complete')
~~~
