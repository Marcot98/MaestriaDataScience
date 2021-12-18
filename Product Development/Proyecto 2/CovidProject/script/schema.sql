CREATE TABLE test.sales(
    id int primary key auto_increment,
    order_number int,
    quantity_ordered int,
    price_each decimal(20,2),
    order_line_number int,
    sales decimal(20,2),
    order_date datetime,
    status varchar(32),
    qtr_id int,
    month_id int,
    year_id int,
    product_line varchar(64),
    msrp int,
    product_code varchar(64),
    customer_name varchar(256),
    phone varchar(32),
    address_line_1 varchar(256),
    address_line_2 varchar(256),
    city varchar(32),
    state varchar(32),
    postal_code varchar(32),
    country varchar(32),
    territory varchar(32),
    contact_last_name varchar(64),
    contact_first_name varchar(64),
    deal_size varchar(32)
);

CREATE TABLE test.consolidate_sales(
id int primary key auto_increment,
year_id int,
month_id int,
sales_amount decimal(20,2)
);