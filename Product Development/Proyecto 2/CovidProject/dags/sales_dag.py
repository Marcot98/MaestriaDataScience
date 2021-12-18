import os

from airflow import DAG
from airflow.contrib.hooks.fs_hook import FSHook
from airflow.contrib.sensors.file_sensor import FileSensor
from airflow.hooks.mysql_hook import MySqlHook
from airflow.operators.python_operator import PythonOperator
from airflow.utils.dates import days_ago
from structlog import get_logger
import pandas as pd

logger = get_logger()

COLUMNS = {
    "ORDERNUMBER": "order_number",
    "QUANTITYORDERED": "quantity_ordered",
    "PRICEEACH": "price_each",
    "ORDERLINENUMBER": "order_line_number",
    "SALES": "sales",
    "ORDERDATE": "order_date",
    "STATUS": "status",
    "QTR_ID": "qtr_id",
    "MONTH_ID": "month_id",
    "YEAR_ID": "year_id",
    "PRODUCTLINE": "product_line",
    "MSRP": "msrp",
    "PRODUCTCODE": "product_code",
    "CUSTOMERNAME": "customer_name",
    "PHONE": "phone",
    "ADDRESSLINE1": "address_line_1",
    "ADDRESSLINE2": "address_line_2",
    "CITY": "city",
    "STATE": "state",
    "POSTALCODE": "postal_code",
    "COUNTRY": "country",
    "TERRITORY": "territory",
    "CONTACTLASTNAME": "contact_last_name",
    "CONTACTFIRSTNAME": "contact_first_name",
    "DEALSIZE": "deal_size"
}

DATE_COLUMNS = ["ORDERDATE"]

FILE_CONNECTION_NAME = 'monitor_file'
CONNECTION_DB_NAME = 'mysql_db'

def etl_process(**kwargs):
    logger.info(kwargs["execution_date"])
    file_path = FSHook(FILE_CONNECTION_NAME).get_path()
    filename = 'sales.csv'
    mysql_connection = MySqlHook(mysql_conn_id=CONNECTION_DB_NAME).get_sqlalchemy_engine()
    full_path = f'{file_path}/{filename}'
    df = (pd.read_csv(full_path, encoding = "ISO-8859-1", usecols=COLUMNS.keys(), parse_dates=DATE_COLUMNS)
          .rename(columns=COLUMNS)
          )

    with mysql_connection.begin() as connection:
        connection.execute("DELETE FROM test.sales WHERE 1=1")
        df.to_sql('sales', con=connection, schema='test', if_exists='append', index=False)

    os.remove(full_path)

    logger.info(f"Rows inserted {len(df.index)}")





dag = DAG('sales_ingestion_dag', description='Dag to Ingest Sales',
          default_args={
              'owner': 'obed.espinoza',
              'depends_on_past': False,
              'max_active_runs': 1,
              'start_date': days_ago(5)
          },
          schedule_interval='0 1 * * *',
          catchup=False)

sensor = FileSensor(task_id="file_sensor_task",
                    dag=dag,
                    filepath='sales.csv',
                    fs_conn_id=FILE_CONNECTION_NAME,
                    poke_interval=10,
                    timeout=600)

etl = PythonOperator(task_id="sales_etl",
                     provide_context=True,
                     python_callable=etl_process,
                     dag=dag
                     )

sensor >> etl
