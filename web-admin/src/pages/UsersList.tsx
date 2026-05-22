import { useEffect, useState } from "react";
import {
  Table,
  Button,
  Space,
  Typography,
  Modal,
  Form,
  Input,
  Switch,
  Tag,
  App,
} from "antd";
import { useNavigate } from "react-router-dom";
import { adminApi, User } from "../api/client";

export default function UsersList() {
  const nav = useNavigate();
  const { message, modal } = App.useApp();
  const [users, setUsers] = useState<User[]>([]);
  const [loading, setLoading] = useState(false);
  const [open, setOpen] = useState(false);
  const [form] = Form.useForm();

  const refresh = async () => {
    setLoading(true);
    try {
      const us = await adminApi.listUsers();
      setUsers(us);
    } catch (e: any) {
      message.error(`Failed to load: ${e?.message ?? "unknown"}`);
    } finally {
      setLoading(false);
    }
  };
  useEffect(() => {
    refresh();
  }, []);

  const onDelete = async (u: User) => {
    modal.confirm({
      title: `Delete user "${u.username}"?`,
      content: "All tokens for this user will be revoked (cascade).",
      okType: "danger",
      onOk: async () => {
        try {
          await adminApi.deleteUser(u.id);
          message.success("Deleted");
          refresh();
        } catch (e: any) {
          message.error(`Failed: ${e?.response?.data?.message ?? e?.message}`);
        }
      },
    });
  };

  const onCreate = async (vals: any) => {
    try {
      await adminApi.createUser({
        username: vals.username,
        email: vals.email || null,
        is_admin: !!vals.is_admin,
      });
      message.success("User created");
      setOpen(false);
      form.resetFields();
      refresh();
    } catch (e: any) {
      const msg = e?.response?.data?.message ?? e?.message;
      message.error(`Failed: ${msg}`);
    }
  };

  return (
    <>
      <div className="flex items-center justify-between mb-4">
        <Typography.Title level={3} style={{ margin: 0 }}>
          Users
        </Typography.Title>
        <Button
          type="primary"
          onClick={() => setOpen(true)}
          data-testid="users-new"
        >
          New user
        </Button>
      </div>
      <Table
        rowKey="id"
        loading={loading}
        dataSource={users}
        pagination={{ pageSize: 20 }}
        data-testid="users-table"
        columns={[
          { title: "Username", dataIndex: "username" },
          { title: "Email", dataIndex: "email", render: (v) => v ?? "—" },
          {
            title: "Admin",
            dataIndex: "is_admin",
            render: (v) =>
              v ? <Tag color="gold">admin</Tag> : <Tag>user</Tag>,
          },
          { title: "Created", dataIndex: "created_at" },
          {
            title: "Actions",
            render: (_, u) => (
              <Space>
                <Button onClick={() => nav(`/users/${u.id}`)}>Edit</Button>
                <Button onClick={() => nav(`/users/${u.id}/tokens`)}>
                  Tokens
                </Button>
                <Button danger onClick={() => onDelete(u)}>
                  Delete
                </Button>
              </Space>
            ),
          },
        ]}
      />
      <Modal
        open={open}
        title="New user"
        onCancel={() => setOpen(false)}
        onOk={() => form.submit()}
        okText="Create"
      >
        <Form form={form} layout="vertical" onFinish={onCreate}>
          <Form.Item
            label="Username"
            name="username"
            rules={[{ required: true, message: "Username is required" }]}
          >
            <Input data-testid="new-user-username" />
          </Form.Item>
          <Form.Item label="Email" name="email">
            <Input data-testid="new-user-email" />
          </Form.Item>
          <Form.Item
            label="Admin"
            name="is_admin"
            valuePropName="checked"
            extra="Only bootstrap admin token may set this."
          >
            <Switch data-testid="new-user-admin" />
          </Form.Item>
        </Form>
      </Modal>
    </>
  );
}
