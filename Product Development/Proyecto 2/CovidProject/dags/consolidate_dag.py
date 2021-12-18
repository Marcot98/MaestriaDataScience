from datetime import timedelta

from airflow import DAG
from airflow.hooks.mysql_hook import MySqlHook
from airflow.operators.python_operator import PythonOperator
from airflow.sensors.external_task_sensor import ExternalTaskSensor
from airflow.utils.dates import days_ago
from structlog import get_logger
import pandas as pd

logger = get_logger()


QUERY = """
SELECT 
    year_id, month_id, SUM(sales) AS sales_amount
FROM
    test.sales
GROUP BY 1 , 2
"""
CONNECTION_DB_NAME = 'mysql_db'

def etl_process(**kwargs):
    logger.info(kwargs["execution_date"])
    mysql_connection = MySqlHook(mysql_conn_id=CONNECTION_DB_NAME).get_sqlalchemy_engine()

    df = pd.read_sql(QUERY, con=mysql_connection, coerce_float=False)

    with mysql_connection.begin() as connection:
        connection.execute("DELETE FROM test.consolidate_sales WHERE 1=1")
        df.to_sql("consolidate_sales", con=connection, schema="test", if_exists="append", index=False)

    logger.info(f"Rows inserted {len(df.index)}")



dag = DAG('consolidate_dag', description='Dag to Consolidate Sales',
          default_args={
              'owner': 'obed.espinoza',
              'depends_on_past': False,
              'max_active_runs': 1
          },
          start_date=days_ago(5),
          schedule_interval='0 0 * * *',
          catchup=False)

sensor = ExternalTaskSensor(task_id="sales_etl_sensor",
                            external_dag_id="sales_ingestion_dag",
                            external_task_id="sales_etl",
                            execution_date_fn=lambda dt: dt + timedelta(hours=1))

etl = PythonOperator(task_id="consolidate_task",
                     provide_context=True,
                     python_callable=etl_process,
                     dag=dag
                     )

sensor >> etl
